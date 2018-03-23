000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC75R
000300000000      *  Descrizione   :  Gestione dettaglio competenze di conto economico
000400000000      *                   Programma di visualizzazione subfile e scelta
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  27 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC75D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC75DVS:SFLULT)
001400000000     F                                     SFILE(EC75DVM:MSGULT)
001500000000     FECECD01L  IF   E           K DISK
001600000000     FECECT01L  IF   E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002300000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002400000000     D FUN             S             40    DIM(31)
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D FFU             S              6    DIM(31)
002700000000     D FDE             S             19    DIM(31)
002800000000     D FAT             S              1    DIM(31)
002900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003000000000     D OPD             S              2    DIM(24)
003100000000     D OPS             S              2    DIM(24)
003200000000     D ODE             S             19    DIM(24)
003300000000     D OAT             S              1    DIM(24)
003400000000     D* TABELLA DESCRIZIONI A VIDEO
003500000000     D FUV             S             19    DIM(8)
003600000000     D OPV             S             19    DIM(8)
003700000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003800000000     D KEYS            S              1    DIM(40)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*---------------------
004300000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004400000000     D*---------------------
004500000000     D DSOPZ         E DS
004600000000     D  OPZRIG                 1     40
004700000000     D*---------------------
004800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004900000000     D*---------------------
005000000000     D DSFUN         E DS
005100000000     D  FUNRIG                 1     40
005200000000     D*---------------------
005300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005400000000     D*---------------------
005500000000     D DSMSG         E DS
005600000000     D*---------------------
005700020923     D* DS REPERIMENTO DATI UTENTE
005800020923     D*-------------------
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
011800000000     C*
011900000000IF  1C     $F03          IFEQ      '1'
012000000000     C                   MOVEL     '1'           D60F03
012100000000E   1C                   ENDIF
012200000000     C* FINE PGM
012300000000     C                   MOVEL     DS60          KPJBU
012400000000     C                   SETON                                        LR
012500000000     C*------------------------------------------------------------------------*
012600000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012700000000     C*------------------------------------------------------------------------*
012800000000     C     CARVS         BEGSR
012900000000     C*
013000000000     C                   MOVEL     'VS'          $GES
013100000000     C*
013200000000     C                   EXSR      CAROPZ
013300000000     C                   EXSR      OUTOPZ
013400000000     C                   MOVEA     FUN2          FUN
013500000000     C                   EXSR      CARFUN
013600000000     C                   EXSR      OUTFUN
013700000000     C*
013800000000     C                   WRITE     EC75DVT
013900000000     C                   WRITE     EC75DVO
014000000000     C                   WRITE     EC75DVF
014100000000     C*
014200000000     C                   EXSR      POSSFL
014300000000     C*
014400000000     C                   ENDSR
014500000000     C*------------------------------------------------------------------------*
014600000000     C* GESVS  - GESTIONE SUBFILE
014700000000     C*------------------------------------------------------------------------*
014800000000     C     GESVS         BEGSR
014900000000     C*
015000000000IF  1C     $TESVD        IFEQ      '1'
015100000000     C                   WRITE     EC75DVT
015200000000     C                   WRITE     EC75DVO
015300000000     C                   WRITE     EC75DVF
015400000000E   1C                   ENDIF
015500000000     C*
015600000000     C                   EXSR      OUTSFL
015700000000     C*
015800000000     C                   EXSR      CONFUN
015900000000     C*
016000000000IF  1C     $ERR          IFEQ      '0'
016100000000SL  2C                   SELECT
016200000000     C     KEY           WHENEQ    F3
016300000000     C                   EXSR      F03GES
016400000000     C     KEY           WHENEQ    F6
016500000000     C                   EXSR      F06GES
016600000000     C                   OTHER
016700000000     C                   EXSR      FUNSFL
016800000000E   2C                   ENDSL
016900000000E   1C                   ENDIF
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> USCITA
017400000000     C*------------------------------------------------------------------------*
017500000000     C     F03GES        BEGSR
017600000000     C*
017700000000     C* SE SONO IN SCELTA (OPZIONE 1), QUINDI RICHIAMATO DA UN ALTRO PGM CON
017800000000     C* PUNTO INTERROGATIVO, USO F3 COME F12
017900000000     C     D60OP1        IFNE      'O01'
018000000000     C*
018100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
018200000000     C                   MOVEL     '1'           $F03
018300000000     C                   ENDIF
018400000000     C*
018500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
018600000000     C                   MOVEL     '1'           $FINE
018700000000     C*
018800000000     C                   ENDSR
018900000000     C*------------------------------------------------------------------------*
019000000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019100000000     C*------------------------------------------------------------------------*
019200000000     C     F06GES        BEGSR
019300000000     C*
019400000000     C* LANCIO L'INSERIMENTO
019500000000     C                   Z-ADD     *ZEROS        WCMP
019600940719     C                   MOVEL     *BLANKS       WFRA
019700940726     C                   MOVEL     *BLANKS       WIOE
019800000000     C                   Z-ADD     VCCNBC        WNBC
019900000000     C                   Z-ADD     *ZERO         WKCC
020000000000     C                   Z-ADD     *ZERO         WKSC
020100000000     C                   MOVE      *BLANKS       WRIC
020200940719     C                   Z-ADD     *ZEROS        WCCF
020300000000     C                   Z-ADD     *ZEROS        WDDO
020400000000     C*
020500000000     C                   MOVEL     D60OP0        WOP0
020600000000     C                   MOVEL     'F06'         WOP1
020700000000     C                   MOVEL     'TREC76R'     WPGM              8
020800000000     C                   EXSR      LANPGM
020900000000     C*
021000940722     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
021100940722     C                   Z-ADD     D60NBC        VCCNBC
021200940722     C                   Z-ADD     1             SWMODI
021300940722     C*
021400940722     C* RICARICO IL SUBFILE
021500000000     C                   EXSR      POSSFL
021600000000     C*
021700000000     C                   ENDSR
021800000000     C*------------------------------------------------------------------------*
021900000000     C*  GESOPZ - GESTIONE OPZIONI
022000000000     C*------------------------------------------------------------------------*
022100000000     C     GESOPZ        BEGSR
022200000000     C*
022300000000SL  1C                   SELECT
022400000000     C     VSCOPZ        WHENEQ    ' 2'
022500000000     C                   EXSR      O02GES
022600000000     C     VSCOPZ        WHENEQ    ' 4'
022700000000     C                   EXSR      O04GES
022800000000     C     VSCOPZ        WHENEQ    ' 5'
022900000000     C                   EXSR      O05GES
023000000000     C     VSCOPZ        WHENEQ    ' 7'
023100000000     C                   EXSR      O07GES
023200000000     C                   OTHER
023300000000     C                   MOVEL     MSG(4)        $MSG
023400000000     C                   EXSR      CARMSG
023500000000E   1C                   ENDSL
023600000000     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
024000000000     C*------------------------------------------------------------------------*
024100000000     C     O02GES        BEGSR
024200000000     C*
024300000000IF  1C     VSCATB        IFEQ      'A'
024400000000     C                   MOVEL     '1'           $ERR
024500000000     C                   MOVEL     MSG(12)       $MSG
024600000000     C                   EXSR      CARMSG
024700000000E   1C                   ENDIF
024800000000     C*
024900000000IF  1C     $ERR          IFEQ      '0'
025000000000     C*
025100000000     C* LANCIO LA MODIFICA
025200000000     C                   Z-ADD     VSCCMP        WCMP
025300940719     C                   MOVEL     VSCFRA        WFRA
025400940726     C                   MOVEL     VSCIOE        WIOE
025500000000     C                   Z-ADD     VCCNBC        WNBC
025600000000     C                   Z-ADD     VSCKCC        WKCC
025700000000     C                   Z-ADD     VSCKSC        WKSC
025800000000     C                   MOVE      VSCRIC        WRIC
025900940719     C                   Z-ADD     VSCCCF        WCCF
026000000000     C                   Z-ADD     VSCDDO        WDDO
026100000000     C*
026200000000     C                   MOVEL     D60OP0        WOP0
026300000000     C                   MOVEL     'O02'         WOP1
026400000000     C                   MOVEL     'TREC76R'     WPGM              8
026500000000     C                   EXSR      LANPGM
026600000000     C*
026700940722     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
026800940722     C                   Z-ADD     D60NBC        VCCNBC
026900000000     C                   Z-ADD     1             SWMODI
027000000000     C*
027100000000E   1C                   ENDIF
027200000000     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
027600000000     C*------------------------------------------------------------------------*
027700000000     C     O04GES        BEGSR
027800000000     C*
027900000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
028000000000IF  1C     VSCATB        IFEQ      'A'
028100000000     C                   MOVEL     '1'           $ERR
028200000000     C                   MOVEL     MSG(12)       $MSG
028300000000     C                   EXSR      CARMSG
028400000000E   1C                   ENDIF
028500000000     C*
028600000000IF  1C     $ERR          IFEQ      '0'
028700000000     C*
028800000000     C* LANCIO LA CANCELLAZIONE
028900000000     C                   Z-ADD     VSCCMP        WCMP
029000940719     C                   MOVEL     VSCFRA        WFRA
029100940726     C                   MOVEL     VSCIOE        WIOE
029200000000     C                   Z-ADD     VCCNBC        WNBC
029300000000     C                   Z-ADD     VSCKCC        WKCC
029400000000     C                   Z-ADD     VSCKSC        WKSC
029500000000     C                   MOVE      VSCRIC        WRIC
029600940719     C                   Z-ADD     VSCCCF        WCCF
029700000000     C                   Z-ADD     VSCDDO        WDDO
029800000000     C*
029900000000     C                   MOVEL     D60OP0        WOP0
030000000000     C                   MOVEL     'O04'         WOP1
030100000000     C                   MOVEL     'TREC76R'     WPGM              8
030200000000     C                   EXSR      LANPGM
030300000000     C*
030400940722     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
030500940722     C                   Z-ADD     D60NBC        VCCNBC
030600940722     C                   Z-ADD     1             SWMODI
030700000000     C*
030800000000E   1C                   ENDIF
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031300000000     C*------------------------------------------------------------------------*
031400000000     C     O05GES        BEGSR
031500000000     C*
031600000000     C* LANCIO LA VISUALIZZAZIONE
031700000000     C                   Z-ADD     VSCCMP        WCMP
031800940719     C                   MOVEL     VSCFRA        WFRA
031900940726     C                   MOVEL     VSCIOE        WIOE
032000000000     C                   Z-ADD     VCCNBC        WNBC
032100000000     C                   Z-ADD     VSCKCC        WKCC
032200000000     C                   Z-ADD     VSCKSC        WKSC
032300000000     C                   MOVE      VSCRIC        WRIC
032400940719     C                   Z-ADD     VSCCCF        WCCF
032500000000     C                   Z-ADD     VSCDDO        WDDO
032600000000     C*
032700000000     C                   MOVEL     D60OP0        WOP0
032800000000     C                   MOVEL     'O05'         WOP1
032900000000     C                   MOVEL     'TREC76R'     WPGM              8
033000000000     C                   EXSR      LANPGM
033100000000     C*
033200000000     C                   ENDSR
033300000000     C*------------------------------------------------------------------------*
033400000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
033500000000     C*------------------------------------------------------------------------*
033600000000     C     O07GES        BEGSR
033700000000     C*
033800000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033900000000IF  1C     VSCATB        IFNE      'A'
034000000000     C                   MOVEL     '1'           $ERR
034100000000     C                   MOVEL     MSG(13)       $MSG
034200000000     C                   EXSR      CARMSG
034300000000E   1C                   ENDIF
034400000000     C*
034500000000IF  1C     $ERR          IFEQ      '0'
034600000000     C*
034700000000     C* LANCIO IL RIPRISTINO
034800000000     C                   Z-ADD     VSCCMP        WCMP
034900940719     C                   MOVEL     VSCFRA        WFRA
035000940726     C                   MOVEL     VSCIOE        WIOE
035100000000     C                   Z-ADD     VCCNBC        WNBC
035200000000     C                   Z-ADD     VSCKCC        WKCC
035300000000     C                   Z-ADD     VSCKSC        WKSC
035400000000     C                   MOVE      VSCRIC        WRIC
035500940719     C                   Z-ADD     VSCCCF        WCCF
035600000000     C                   Z-ADD     VSCDDO        WDDO
035700000000     C*
035800000000     C                   MOVEL     D60OP0        WOP0
035900000000     C                   MOVEL     'O07'         WOP1
036000000000     C                   MOVEL     'TREC76R'     WPGM              8
036100000000     C                   EXSR      LANPGM
036200000000     C*
036300940722     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
036400940722     C                   Z-ADD     D60NBC        VCCNBC
036500940722     C                   Z-ADD     1             SWMODI
036600000000     C*
036700000000E   1C                   ENDIF
036800000000     C*
036900000000     C                   ENDSR
037000000000     C*------------------------------------------------------------------------*
037100000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
037200000000     C*------------------------------------------------------------------------*
037300000000     C     NEWPOS        BEGSR
037400000000     C*
037500000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
037600000000     C*        1 = RICHIESTO POSIZIONAMENTO
037700000000     C                   Z-ADD     0             SWPOSI            7 0
037800000000     C*
037900000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
038000000000     C*            NON OCCORRE RICARICARE IL SUBFILE
038100000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038200000000     C                   Z-ADD     0             SWMODI            1 0
038300000000     C*
038400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
038500000000IF  1C     VCCCMP        IFNE      DEPCMP
038600940719     C     VCCFRA        ORNE      DEPFRA
038700940725     C     VCCIOE        ORNE      DEPIOE
038800000000     C     VCCKCC        ORNE      DEPKCC
038900000000     C     VCCKSC        ORNE      DEPKSC
039000000000     C     VCCDDO        ORNE      DEPDDO
039001160211     C     VCCVAL        ORNE      DEPVAL
039100000000     C*
039200000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
039300000000     C                   EXSR      CHKPAR
039400000000     C*
039500000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
039600000000IF  2C     $ERR          IFEQ      '0'
039700000000     C                   Z-ADD     1             SWPOSI
039800000000     C                   Z-ADD     VCCCMP        DEPCMP
039900940719     C                   MOVEL     VCCFRA        DEPFRA
040000940725     C                   MOVEL     VCCIOE        DEPIOE
040100000000     C                   Z-ADD     VCCKCC        DEPKCC
040200000000     C                   Z-ADD     VCCKSC        DEPKSC
040300000000     C                   Z-ADD     VCCDDO        DEPDDO
040301160211     C                   Z-ADD     VCCVAL        DEPVAL
040400000000E   2C                   ENDIF
040500000000E   1C                   ENDIF
040600000000     C*
040700000000     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
041000000000     C*------------------------------------------------------------------------*
041100000000     C     CHKPAR        BEGSR
041200000000     C*
041300000000     C* REIMPOSTO INDICATORI DI ERRORE
041400000000     C                   SETOFF                                         81
041500000000     C*--------------------
041600000000     C* DATA DECORRENZA COMPETENZA
041700000000     C*--------------------
041800000000     C                   Z-ADD     *ZERO         DDOVCC
041900000000IF  1C     VCCDDO        IFGT      *ZEROS
042000000000     C                   Z-ADD     VCCDDO        G02DAT
042100940722     C                   MOVEL     01            G02DAT
042200000000     C                   Z-ADD     *ZEROS        G02INV
042300000000     C                   MOVEL     '0'           G02ERR
042400000000     C                   CALL      'XSRDA8'
042500000000     C                   PARM                    WLBDAT
042600000000     C*
042700000000IF  2C     G02ERR        IFEQ      '1'
042800000000     C                   MOVEL     '1'           $ERR
042900000000     C                   MOVEL     MSG(9)        $MSG
043000000000     C                   EXSR      CARMSG
043100000000     C                   SETON                                        81
043200000000X   2C                   ELSE
043300000000     C                   Z-ADD     G02DAT        VCCDDO
043400940722     C                   MOVE      00            G02INV
043500000000     C                   Z-ADD     G02INV        DDOVCC            8 0
043600000000E   2C                   ENDIF
043700000000E   1C                   ENDIF
043800000000     C*
043801160211     C*--------------------
043802160211     C* DATA VALIDITÀ
043803160211     C*--------------------
043804160211     C                   Z-ADD     *ZERO         VALVCC
043805160211IF  1C     VCCVAL        IFGT      *ZEROS
043806160211     C                   Z-ADD     VCCVAL        G02DAT
043807160211     C                   MOVEL     01            G02DAT
043808160211     C                   Z-ADD     *ZEROS        G02INV
043809160211     C                   MOVEL     '0'           G02ERR
043810160211     C                   CALL      'XSRDA8'
043811160211     C                   PARM                    WLBDAT
043812160211     C*
043813160211IF  2C     G02ERR        IFEQ      '1'
043814160211     C                   MOVEL     '1'           $ERR
043815160211     C                   MOVEL     MSG(9)        $MSG
043816160211     C                   EXSR      CARMSG
043817160211     C                   SETON                                        82
043818160211X   2C                   ELSE
043819160211     C                   Z-ADD     G02DAT        VCCVAL
043820160211     C                   MOVE      00            G02INV
043821160211     C                   Z-ADD     G02INV        VALVCC            8 0
043822160211E   2C                   ENDIF
043823160211E   1C                   ENDIF
043900000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
044000000000     C* SUL CAMPO DEL CONTROL
044100000000IF  1C     $ERR          IFEQ      '1'
044200000000     C                   MOVEL     '0'           *IN43
044300000000X   1C                   ELSE
044400000000     C                   MOVEL     '1'           *IN43
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C                   ENDSR
044800000000     C*------------------------------------------------------------------------*
044900000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
045000000000     C*------------------------------------------------------------------------*
045100000000     C     SETFIL        BEGSR
045200000000     C*
045300000000     C                   MOVEL     '1'           *IN43
045400000000     C*
045500000000     C* POSIZIONAMENTO ARCHIVIO
045600000000     C                   Z-ADD     VCCNBC        KCDNBC
045700000000     C                   Z-ADD     VCCCMP        KCDCMP
045800940719     C                   MOVEL     VCCFRA        KCDFRA
045900940725     C                   MOVEL     VCCIOE        KCDIOE
046000000000     C                   Z-ADD     VCCKCC        KCDKCC
046100000000     C                   Z-ADD     VCCKSC        KCDKSC
046200000000     C     KEYECD        SETLL     ECECD01L                           99
046300000000     C*
046400000000     C  N99              EXSR      LETFIL
046500000000     C*
046600000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
046700000000IF  1C     *IN99         IFEQ      '1'
046800000000     C                   Z-ADD     1             SFLEOF
046900000000E   1C                   ENDIF
047000000000     C*
047100000000     C                   ENDSR
047200000000     C*------------------------------------------------------------------------*
047300000000     C* LETFIL - LEGGE RECORD SUCESSIVO
047400000000     C*------------------------------------------------------------------------*
047500000000     C     LETFIL        BEGSR
047600000000     C*
047700000000     C* LEGGO FINO A:
047800000000     C*     - FINE FILE
047900000000     C*     - TROVATO RECORD VALIDO
048000000000     C                   MOVEL     '0'           WRECOK            1
048100000000DO  1C     *IN99         DOWEQ     *OFF
048200000000     C     WRECOK        ANDEQ     '0'
048300000000     C*
048400000000     C* LETTURA
048500000000     C                   READ      ECECD01L                               99
048600000000     C*
048700000000     C* FINE FILE  O TABELLA DIVERSA
048800000000IF  2C     *IN99         IFEQ      '1'
048900000000     C                   Z-ADD     1             SFLEOF
049000000000X   2C                   ELSE
049100000000     C                   EXSR      CHKREC
049200000000E   2C                   ENDIF
049300000000     C*
049400000000E   1C                   ENDDO
049500000000     C*
049600000000     C                   ENDSR
049700000000     C*------------------------------------------------------------------------*
049800000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
049900000000     C*------------------------------------------------------------------------*
050000000000     C     CHKREC        BEGSR
050100000000     C*
050200000000     C                   MOVEL     '1'           WRECOK
050300000000     C*
050400000000     C* INIZIO ELENCO DA  ====>  CODICE CONTO DI CONTABILITA'
050500000000IF  1C     VCCKCC        IFGT      *ZEROS
050600160211IF  2C     VCCKCC        IFne      ECDKCC
050700000000     C                   MOVEL     '0'           WRECOK
050800000000E   2C                   ENDIF
050900000000E   1C                   ENDIF
051000000000     C*
051100000000     C* INIZIO ELENCO DA  ====>  CODICE SOTTOCONTO DI CONTABILITA'
051200000000IF  1C     VCCKSC        IFGT      *ZEROS
051300160211IF  2C     VCCKSC        IFne      ECDKSC
051400000000     C                   MOVEL     '0'           WRECOK
051500000000E   2C                   ENDIF
051600000000E   1C                   ENDIF
051700000000     C*
051800000000     C* SELEZIONE PER  ====>  DATA DECORRENZA
051900000000IF  1C     DDOVCC        IFGT      *ZEROS
052000000000IF  2C     DDOVCC        IFNE      ECDDDO
052100000000     C                   MOVEL     '0'           WRECOK
052200000000E   2C                   ENDIF
052300000000E   1C                   ENDIF
052301160211     C* SELEZIONE PER  ====>  DATA VALIDITÀ
052302160211IF  1C     VALVCC        IFGT      *ZEROS
052303160211IF  2C     VALVCC        IFLT      ECDDDO
052304160211IF  2C     VALVCC        ORgt      ECDDSO
052305160211     C                   MOVEL     '0'           WRECOK
052306160211E   2C                   ENDIF
052307160211E   1C                   ENDIF
052308151009     C* SELEZIONE PER  ====>  italia / estero
052309151009IF  1C     VCCIOE        IFne      ' '
052310151009IF  2C     VCCIOE        IFNE      ECDioe
052311151009     C                   MOVEL     '0'           WRECOK
052312151009E   2C                   ENDIF
052313151009E   1C                   ENDIF
052314151009     C* SELEZIONE PER  ====>  franchi / assegnati
052315151009IF  1C     VCCfra        IFne      ' '
052316151009IF  2C     VCCfra        IFNE      ECDfra
052317151009     C                   MOVEL     '0'           WRECOK
052318151009E   2C                   ENDIF
052319151009E   1C                   ENDIF
052400000000     C*
052500000000     C                   ENDSR
052600000000     C*------------------------------------------------------------------------*
052700000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
052800000000     C*------------------------------------------------------------------------*
052900000000     C     IRESFL        BEGSR
053000000000     C*
053100000000IF  1C     ECDATB        IFNE      *BLANKS
053200000000     C                   MOVEL     'A'           VSCATB
053300000000X   1C                   ELSE
053400000000     C                   MOVEL     ' '           VSCATB
053500000000E   1C                   ENDIF
053600000000     C*
053700000000     C* DECODIFICO LA COMPETENZA
053800000000     C                   Z-ADD     VCCNBC        KCTNBC
053900000000     C                   Z-ADD     ECDCMP        KCTCMP
054000040209     C     KEYECT        CHAIN     ECECT01L
054100000000     C*
054101040209     c                   if        %found(ecect01l)
054200000000     C                   MOVEL     ECTCMD        VSCCMD
054201040209     c                   else
054202040209     C                   MOVEL     *all'*'       VSCCMD
054203040209     c                   end
054300000000     C                   Z-ADD     ECDCMP        VSCCMP
054400940719     C                   MOVEL     ECDFRA        VSCFRA
054500940726     C                   MOVEL     ECDIOE        VSCIOE
054600000000     C                   Z-ADD     ECDKCC        VSCKCC
054700000000     C                   Z-ADD     ECDKSC        VSCKSC
054800000000     C                   MOVE      ECDRIC        VSCRIC
054900940719     C                   Z-ADD     ECDCCF        VSCCCF
055000000000     C                   Z-ADD     ECDDDO        G02INV
055100000000     C                   Z-ADD     *ZEROS        G02DAT
055200000000     C                   MOVEL     '3'           G02ERR
055300000000     C                   CALL      'XSRDA8'
055400000000     C                   PARM                    WLBDAT
055500940719     C                   Z-ADD     G02DAT        VSCDDO
055600000000     C*
055700000000     C                   ENDSR
055800000000     C*------------------------------------------------------------------------*
055900000000     C* LANPGM - LANCIA UN PROGRAMMA
056000000000     C*------------------------------------------------------------------------*
056100000000     C     LANPGM        BEGSR
056200000000     C*--------------------
056300000000     C* PARAMETRI INFORMATIVI
056400000000     C*--------------------
056500000000     C                   MOVEL     WOP0          D60OP0
056600000000     C                   MOVEL     WOP1          D60OP1
056700000000     C                   MOVEL     '0'           D60F03
056800000000     C                   MOVEL     '0'           D60ERR
056900000000     C                   MOVEL     *BLANKS       D60MSG
057000000000     C*--------------------
057100000000     C* PARAMETRI
057200000000     C*--------------------
057300000000     C                   Z-ADD     WCMP          D60CMP
057400940719     C                   MOVEL     WFRA          D60FRA
057500940726     C                   MOVEL     WIOE          D60IOE
057600000000     C                   Z-ADD     WNBC          D60NBC
057700000000     C                   Z-ADD     WKCC          D60KCC
057800000000     C                   Z-ADD     WKSC          D60KSC
057900000000     C                   MOVE      WRIC          D60RIC
058000940719     C                   Z-ADD     WCCF          D60CCF
058100000000     C                   Z-ADD     WDDO          D60DDO
058200000000     C* LANCIO
058300000000     C                   MOVEL     DS60          KPJBU
058400000000     C                   CALL      WPGM                                 99
058500000000     C                   PARM                    KPJBA
058600000000     C                   MOVEL     KPJBU         DS60
058700000000     C                   Z-ADD     D60NBC        VCCNBC
058800000000     C*
058900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
059000000000IF  1C     D60F03        IFEQ      '1'
059100000000     C                   MOVEL     '1'           $F03
059200000000     C                   MOVEL     '1'           $ERR
059300000000     C                   MOVEL     '1'           $FINE
059400000000     C*
059500000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
059600000000X   1C                   ELSE
059700000000     C                   MOVEL     D60MSG        $MSG
059800000000     C                   EXSR      CARMSG
059900000000E   1C                   ENDIF
060000000000     C*
060100000000     C                   ENDSR
060200020923     C*-------------------------------------------------------------------------
060300020923     C* REPERISCE I DATI UTENTE
060400020923     C*-------------------------------------------------------------------------
060500020923     C     REPDATIUTE    BEGSR
060600020923     C*
060700020923     C* INIZIALIZZA VARIABILI DI WRK
060800020923     C                   CLEAR                   TIBS34DS
060900020923     C                   CLEAR                   AZUTEDS
061000020923     C                   CLEAR                   DDATIUTE
061100020923     C*
061200020923     C     *DTAARA       DEFINE    §azute        azuteds
061300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
061400020923     C                   IN(E)     *DTAARA
061500020923if  1C                   IF        %Error
061600020923     c                   EVAL      I34Tla = 'L'
061700020923     C                   CALL      'TIBS34R'
061800020923     C                   PARM                    Tibs34Ds
061900020923     C                   IN        *DTAARA
062000020923e   1C                   ENDIF
062100020923     C*
062200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
062300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
062400020923     C*
062500020923     C                   ENDSR
062600000000     C*------------------------------------------------------------------------*
062700000000     C*  *INZSR - ROUTINE INIZIALE
062800000000     C*------------------------------------------------------------------------*
062900000000     C     *INZSR        BEGSR
063000000000     C*
063100000000     C* RICEVIMENTO PARAMETRI
063200000000     C     *ENTRY        PLIST
063300000000     C                   PARM                    KPJBA
063400000000     C                   MOVEL     KPJBU         DS60
063500000000     C*
063600000000     C* DEFINIZIONE KLIST
063700000000     C     *LIKE         DEFINE    ECDNBC        KCDNBC
063800000000     C     *LIKE         DEFINE    ECDCMP        KCDCMP
063900940719     C     *LIKE         DEFINE    ECDFRA        KCDFRA
064000940725     C     *LIKE         DEFINE    ECDIOE        KCDIOE
064100000000     C     *LIKE         DEFINE    ECDKCC        KCDKCC
064200000000     C     *LIKE         DEFINE    ECDKSC        KCDKSC
064300000000     C     *LIKE         DEFINE    ECDDDO        KCDDDO
064400000000     C     *LIKE         DEFINE    ECDNBC        KCTNBC
064500000000     C     *LIKE         DEFINE    ECDCMP        KCTCMP
064600000000     C*
064700000000     C* DEFINIZIONE DEPOSITI
064800000000     C     *LIKE         DEFINE    VCCCMP        DEPCMP
064900940719     C     *LIKE         DEFINE    VCCFRA        DEPFRA
065000940725     C     *LIKE         DEFINE    VCCIOE        DEPIOE
065100000000     C     *LIKE         DEFINE    VCCKCC        DEPKCC
065200000000     C     *LIKE         DEFINE    VCCKSC        DEPKSC
065300000000     C     *LIKE         DEFINE    VCCDDO        DEPDDO
065301160211     C     *LIKE         DEFINE    VCCVAL        DEPVAL
065400000000     C*
065500000000     C     *LIKE         DEFINE    D60OP0        WOP0
065600000000     C     *LIKE         DEFINE    D60OP1        WOP1
065700000000     C     *LIKE         DEFINE    D60CMP        WCMP
065800940719     C     *LIKE         DEFINE    D60FRA        WFRA
065900940726     C     *LIKE         DEFINE    D60IOE        WIOE
066000000000     C     *LIKE         DEFINE    D60NBC        WNBC
066100000000     C     *LIKE         DEFINE    D60KCC        WKCC
066200000000     C     *LIKE         DEFINE    D60KSC        WKSC
066300000000     C     *LIKE         DEFINE    D60RIC        WRIC
066400940719     C     *LIKE         DEFINE    D60CCF        WCCF
066500000000     C     *LIKE         DEFINE    D60DDO        WDDO
066600000000     C*
066700000000     C* CHIAVE LETTURA ECECD01L - PARZIALE
066800000000     C     KEYECD        KLIST
066900000000     C                   KFLD                    KCDNBC
067000000000     C                   KFLD                    KCDCMP
067100940719     C                   KFLD                    KCDFRA
067200940726     C                   KFLD                    KCDIOE
067300000000     C                   KFLD                    KCDKCC
067400000000     C                   KFLD                    KCDKSC
067500000000     C*
067600000000     C* CHIAVE LETTURA ECECT01L - PARZIALE
067700000000     C     KEYECT        KLIST
067800000000     C                   KFLD                    KCTNBC
067900000000     C                   KFLD                    KCTCMP
068000000000     C*
068100000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
068200000000     C                   MOVE      D60OP0        INTGES            2
068300000000IF  1C     INTGES        IFEQ      '02'
068400000000     C     D60OP1        ANDNE     'O01'
068500000000     C                   MOVEL     'G'           $TGES             1
068600000000     C                   MOVEL     DESGES        VTDOPE
068700000000X   1C                   ELSE
068800000000     C                   MOVEL     'I'           $TGES             1
068900000000     C                   MOVEL     DESINT        VTDOPE
069000000000E   1C                   ENDIF
069100000000     C*
069200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
069300000000     C                   EXSR      INZSTD
069400000000     C*
069500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
069600000000     C                   EXSR      INZSFL
069700000000     C*
069800000000     C* IMPOSTO CAMPI TESTATA VIDEO
069900000000     C                   MOVEL     DSFIRS        VTDRSU
070000000000     C*
070100000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
070200000000     C                   MOVEL     D60OP0        $UNO              1
070300000000IF  1C     $UNO          IFNE      'N'
070400000000     C                   Z-ADD     D60CMP        VCCCMP
070500940719     C                   MOVEL     D60FRA        VCCFRA
070600940725     C                   MOVEL     D60IOE        VCCIOE
070700000000     C                   Z-ADD     D60NBC        VCCNBC
070801160211     C                   Z-ADD     D60DDC        VCCDDO
070900000000     C                   Z-ADD     D60CMP        DEPCMP
071000940719     C                   MOVEL     D60FRA        DEPFRA
071100940725     C                   MOVEL     D60IOE        DEPIOE
071200000000     C                   Z-ADD     D60DDC        DEPDDO
071300000000     C                   Z-ADD     D60DDC        G02DAT
071400940722     C                   MOVEL     01            G02DAT
071500000000     C                   Z-ADD     *ZEROS        G02INV
071600000000     C                   MOVEL     '0'           G02ERR
071700000000     C                   CALL      'XSRDA8'
071800000000     C                   PARM                    WLBDAT
071900940722     C                   MOVE      00            G02INV
072000000000     C                   Z-ADD     G02INV        DDOVCC
072100000000X   1C                   ELSE
072200000000     C                   Z-ADD     *ZERO         VCCCMP
072300940719     C                   MOVEL     *BLANKS       VCCFRA
072400940725     C                   MOVEL     *BLANKS       VCCIOE
072500000000     C                   Z-ADD     *ZERO         VCCNBC
072600000000     C                   Z-ADD     *ZERO         VCCDDO
072601160211     C                   Z-ADD     *ZERO         VCCVAL
072700000000     C                   Z-ADD     *ZERO         DEPCMP
072800940719     C                   MOVEL     *BLANKS       DEPFRA
072900940725     C                   MOVEL     *BLANKS       DEPIOE
073000000000     C                   Z-ADD     *ZERO         DEPDDO
073100000000E   1C                   ENDIF
073200000000     C                   Z-ADD     *ZEROS        VCCKCC
073300000000     C                   Z-ADD     *ZEROS        VCCKSC
073400000000     C                   Z-ADD     *ZEROS        DEPKCC
073500000000     C                   Z-ADD     *ZEROS        DEPKSC
073600000000     C*
073700000000     C* DEVO REPERIRE IL NUMERO BLOCCO, L'ULTIMO IN ARCHIVIO
073800000000     C* SE NON MI E' STATO PASSATO NELLA DS PARAMETRICA
073900000000     C     VCCNBC        IFEQ      *ZERO
074000940722     C     *HIVAL        SETGT     ECECT01L
074100000000     C                   READP     ECECT01L                               99
074200000000     C   99              Z-ADD     1             VCCNBC
074300000000     C  N99              Z-ADD     ECTNBC        VCCNBC
074400000000     C                   ENDIF
074500000000     C*
074600000000     C* EMETTO VIDEATA SUBFILE
074700000000     C                   EXSR      CARVS
074800000000     C*
074900000000     C                   ENDSR
075000000000     C*------------------------------------------------------------------------*
075100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
075400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
075500000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
075600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075800000000     C*------------------------------------------------------------------------*
075900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
076000000000     C*------------------------------------------------------------------------*
076100000000     C     INZSTD        BEGSR
076200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
076300000000     C                   CALL      'XKEYS'
076400000000     C                   PARM                    DSKEYS
076500000000     C                   MOVEA     KEYRIG        KEYS
076600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
076700000000     C     XMSGPA        PLIST
076800000000     C                   PARM                    DSMSMS
076900000000     C                   PARM                    DSMSPG
077000000000     C                   PARM                    DSMSKE
077100000000     C                   PARM                    DSMSER
077200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
077300000000     C                   MOVEL     'TREC75R'     DSMSPG
077400000000     C                   MOVEL     'TREC75R'     VMCQUE
077500000000     C                   MOVEL     *BLANKS       VMCKEY
077600000000     C                   MOVEL     *BLANKS       DSMSMS
077700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
077800000000     C                   MOVEL     '0'           $ERR              1
077900000000     C                   MOVEL     '0'           $F03              1
078000000000     C                   MOVEL     *BLANKS       $GES              2
078100000000     C                   MOVEL     *BLANKS       $MSG             80
078200000000     C                   MOVEL     '0'           $FINE             1
078300000000     C                   MOVEL     '0'           $TESVD            1
078400000000     C* LANCIO ROUTINE PER VERIFICARE SE
078500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
078600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
078700020923     C                   EXSR      REPDATIUTE
078800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
078900000000     C                   BITOFF    '01234567'    POSMI1            1
079000000000     C                   BITOFF    '01234567'    POSMI2            1
079100000000     C                   BITOFF    '01234567'    POSMA1            1
079200000000     C                   BITOFF    '01234567'    POSMA2            1
079300000000     C                   BITON     '34'          POSMI1            1
079400000000     C                   BITON     '7'           POSMI2            1
079500000000     C                   BITON     '34'          POSMA1            1
079600000000     C                   BITON     '0'           POSMA2            1
079700000000     C                   MOVE      POSMI2        POSMIN            2
079800000000     C                   MOVEL     POSMI1        POSMIN
079900000000     C                   MOVE      POSMA2        POSMAX            2
080000000000     C                   MOVEL     POSMA1        POSMAX
080100000000     C                   ENDSR
080200000000     C*------------------------------------------------------------------------*
080300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
080400000000     C*------------------------------------------------------------------------*
080500000000     C     FUNSTD        BEGSR
080600000000     C                   SELECT
080700000000     C     KEY           WHENEQ    F3
080800000000     C                   EXSR      F03STD
080900000000     C     KEY           WHENEQ    F12
081000000000     C                   EXSR      F12STD
081100000000     C     KEY           WHENEQ    F24
081200000000     C                   EXSR      F24STD
081300000000     C                   OTHER
081400000000     C                   MOVEL     MSG(2)        $MSG
081500000000     C                   EXSR      CARMSG
081600000000     C                   ENDSL
081700000000     C                   ENDSR
081800000000     C*------------------------------------------------------------------------*
081900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
082000000000     C*------------------------------------------------------------------------*
082100000000     C     F03STD        BEGSR
082200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
082300000000     C                   MOVEL     '1'           $F03
082400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
082500000000     C                   MOVEL     '1'           $FINE
082600000000     C                   ENDSR
082700000000     C*------------------------------------------------------------------------*
082800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
082900000000     C*------------------------------------------------------------------------*
083000000000     C     F12STD        BEGSR
083100000000     C* IMPOSTO FLAG DI FIN PGM
083200000000     C                   MOVEL     '1'           $FINE
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
083600000000     C*------------------------------------------------------------------------*
083700000000     C     F24STD        BEGSR
083800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
083900000000     C                   EXSR      OUTFUN
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
084300000000     C*------------------------------------------------------------------------*
084400000000     C     INZSFL        BEGSR
084500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
084600000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
084700000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
084800000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
084900000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
085000000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
085100000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
085200000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
085300000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
085400000000     C                   Z-ADD     12            SFLPAG
085500000000     C                   ENDSR
085600000000     C*------------------------------------------------------------------------*
085700000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
085800000000     C*------------------------------------------------------------------------*
085900000000     C     FUNSFL        BEGSR
086000000000     C                   SELECT
086100000000     C     KEY           WHENEQ    ENTER
086200000000     C                   EXSR      ENTSFL
086300000000     C     KEY           WHENEQ    F3
086400000000     C                   EXSR      F03STD
086500000000     C     KEY           WHENEQ    F5
086600000000     C                   EXSR      F05STD
086700000000     C     KEY           WHENEQ    F12
086800000000     C                   EXSR      F12STD
086900000000     C     KEY           WHENEQ    F17
087000000000     C                   EXSR      F17STD
087100000000     C     KEY           WHENEQ    F18
087200000000     C                   EXSR      F18STD
087300000000     C     KEY           WHENEQ    F23
087400000000     C                   EXSR      F23STD
087500000000     C     KEY           WHENEQ    F24
087600000000     C                   EXSR      F24STD
087700000000     C     KEY           WHENEQ    ROLLUP
087800000000     C                   EXSR      RUPSTD
087900000000     C     KEY           WHENEQ    ROLLDN
088000000000     C                   EXSR      RDNSTD
088100000000     C                   OTHER
088200000000     C                   MOVEL     MSG(2)        $MSG
088300000000     C                   EXSR      CARMSG
088400000000     C                   ENDSL
088500000000     C                   ENDSR
088600000000     C*------------------------------------------------------------------------*
088700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
088800000000     C*------------------------------------------------------------------------*
088900000000     C     F05STD        BEGSR
089000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
089100000000     C                   EXSR      POSSFL
089200000000     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
089500000000     C*------------------------------------------------------------------------*
089600000000     C     F17STD        BEGSR
089700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
089800000000     C                   Z-ADD     1             VCCNBR
089900000000     C                   ENDSR
090000000000     C*------------------------------------------------------------------------*
090100000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
090200000000     C*------------------------------------------------------------------------*
090300000000     C     F18STD        BEGSR
090400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
090500000000     C                   Z-ADD     SFLULT        VCCNBR
090600000000     C                   ENDSR
090700000000     C*------------------------------------------------------------------------*
090800000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
090900000000     C*------------------------------------------------------------------------*
091000000000     C     F23STD        BEGSR
091100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
091200000000     C                   EXSR      OUTOPZ
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
091600000000     C*------------------------------------------------------------------------*
091700000000     C     RUPSTD        BEGSR
091800000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
091900000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
092000000000     C     CURSOR        IFGE      POSMIN
092100000000     C     CURSOR        ANDLE     POSMAX
092200000000     C                   ELSE
092300000000     C                   EXSR      CARSFL
092400000000     C                   ENDIF
092500000000     C                   ENDSR
092600000000     C*------------------------------------------------------------------------*
092700000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
092800000000     C*------------------------------------------------------------------------*
092900000000     C     RDNSTD        BEGSR
093000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
093100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
093200000000     C     CURSOR        IFGE      POSMIN
093300000000     C     CURSOR        ANDLE     POSMAX
093400000000     C                   ELSE
093500000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
093600000000     C                   MOVEL     '1'           $ERR
093700000000     C                   MOVEL     MSG(5)        $MSG
093800000000     C                   EXSR      CARMSG
093900000000     C                   ENDIF
094000000000     C                   ENDSR
094100000000     C*------------------------------------------------------------------------*
094200000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
094300000000     C*------------------------------------------------------------------------*
094400000000     C     ENTSFL        BEGSR
094500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
094600000000     C                   EXSR      NEWPOS
094700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
094800000000     C     $ERR          IFEQ      '0'
094900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
095000000000     C     SWPOSI        IFNE      0
095100000000     C                   EXSR      POSSFL
095200000000     C                   ELSE
095300000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
095400000000     C     SFLULT        IFNE      0
095500000000     C                   EXSR      OPZSFL
095600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
095700000000     C     SWMODI        IFNE      0
095800000000     C                   EXSR      POSSFL
095900000000     C                   ENDIF
096000000000     C                   ENDIF
096100000000     C                   ENDIF
096200000000     C                   ENDIF
096300000000     C                   ENDSR
096400000000     C*------------------------------------------------------------------------*
096500000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
096600000000     C*------------------------------------------------------------------------*
096700000000     C     POSSFL        BEGSR
096800000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
096900000000     C                   EXSR      CLRSFL
097000000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
097100000000     C                   EXSR      SETFIL
097200000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
097300000000     C     SFLEOF        IFEQ      0
097400000000     C                   EXSR      CARSFL
097500000000     C                   ENDIF
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
097900000000     C*------------------------------------------------------------------------*
098000000000     C     OPZSFL        BEGSR
098100000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
098200000000     C                   MOVEL     'S'           OPZOK             1
098300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
098400000000     C                   Z-ADD     0             SFLVAR            5 0
098500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098600000000     C                   Z-ADD     0             SFLERR
098700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098800000000     C                   Z-ADD     SFLULT        WFLULT
098900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
099000000000     C                   READC     EC75DVS                                49
099100000000     C     *IN49         DOWEQ     '0'
099200941121     C     $ERR          ANDEQ     '0'
099300000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
099400000000     C                   ADD       1             SFLVAR
099500000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
099600000000     C     OPZOK         IFEQ      'S'
099700000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
099800000000     C     VSCOPZ        IFNE      *BLANKS
099900000000     C                   EXSR      CONOPZ
100000000000     C     OPZOK         IFEQ      'S'
100100941121     C     $ERR          ANDEQ     '0'
100200000000     C                   EXSR      GESOPZ
100300000000     C                   ENDIF
100400000000     C                   ENDIF
100500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100600000000     C*            2. PULIZIA SCELTA IN SFL
100700000000     C     OPZOK         IFEQ      'S'
100800000000     C                   MOVEL     '0'           *IN41
100900000000     C                   MOVEL     *BLANKS       VSCOPZ
101000000000     C                   ELSE
101100000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101200000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101300000000     C                   MOVEL     '1'           *IN41
101400000000     C                   Z-ADD     SFLULT        SFLERR
101500000000     C                   ENDIF
101600000000     C                   ELSE
101700000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101800000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101900000000     C                   MOVEL     '1'           *IN41
102000000000     C                   ENDIF
102100000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102200000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102300000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102400000000     C     OPZOK         IFNE      'S'
102500000000     C                   Z-ADD     SFLERR        VCCNBR
102600000000     C                   ELSE
102700000000     C                   Z-ADD     SFLULT        VCCNBR
102800000000     C                   ENDIF
102900000000     C* AGGIORNO RECORD SFL
103000000000     C                   UPDATE    EC75DVS
103100941121     C     $ERR          IFEQ      '0'
103200000000     C                   READC     EC75DVS                                49
103300941121     C                   ENDIF
103400000000     C                   ENDDO
103500000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103600000000     C                   Z-ADD     WFLULT        SFLULT
103700000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
103800000000     C     SFLVAR        IFEQ      0
103900000000     C                   MOVEL     '1'           $FINE
104000000000     C                   ENDIF
104100000000     C                   ENDSR
104200000000     C*------------------------------------------------------------------------*
104300000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104400000000     C*------------------------------------------------------------------------*
104500000000     C     CLRSFL        BEGSR
104600000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
104700000000     C* (0=NON FINE FILE  1=FINE FILE)
104800000000     C                   Z-ADD     0             SFLEOF
104900000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
105000000000     C                   Z-ADD     0             SFLULT
105100000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
105200000000     C                   MOVEL     '1'           *IN40
105300000000     C                   WRITE     EC75DVC
105400000000     C                   MOVEL     '0'           *IN40
105500000000     C                   ENDSR
105600000000     C*------------------------------------------------------------------------*
105700000000     C* CARSFL - CARICAMENTO SFL
105800000000     C*------------------------------------------------------------------------*
105900000000     C     CARSFL        BEGSR
106000000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
106100000000     C     SFLULT        IFEQ      0
106200000000     C                   EXSR      SETFIL
106300000000     C                   ENDIF
106400000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
106500000000     C                   Z-ADD     SFLULT        WFLULT
106600000000     C* CICLO:
106700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
106800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
106900000000     C                   Z-ADD     1             SFLRIG
107000000000     C     SFLRIG        DOWLE     SFLPAG
107100000000     C     SFLEOF        ANDEQ     0
107200000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
107300000000     C                   MOVEL     *BLANKS       VSCOPZ
107400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
107500000000     C                   EXSR      IRESFL
107600000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
107700000000     C                   ADD       1             SFLULT
107800000000     C* SCRITTURA RECORD SFL
107900000000     C                   WRITE     EC75DVS
108000000000     C                   ADD       1             SFLRIG
108100000000     C* LETTURA FILE DA CARICARE
108200000000     C                   EXSR      LETFIL
108300000000     C                   ENDDO
108400000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
108500000000     C     WFLULT        IFNE      SFLULT
108600000000     C                   Z-ADD     WFLULT        VCCNBR
108700000000     C                   ADD       1             VCCNBR
108800000000     C                   ENDIF
108900000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
109000000000     C     SFLEOF        IFEQ      1
109100000000     C     SFLULT        ANDGT     *ZEROS
109200000000     C                   MOVEL     '1'           $ERR
109300000000     C                   MOVEL     MSG(6)        $MSG
109400000000     C                   EXSR      CARMSG
109500000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
109600000000     C     SFLULT        CHAIN     EC75DVS                            99
109700000000     C     *IN99         IFEQ      '0'
109800000000     C                   SETON                                        45
109900000000     C                   UPDATE    EC75DVS
110000000000     C                   SETOFF                                       45
110100000000     C                   ENDIF
110200000000     C                   ENDIF
110300000000     C                   ENDSR
110400000000     C*------------------------------------------------------------------------*
110500000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
110600000000     C*------------------------------------------------------------------------*
110700000000     C     OUTSFL        BEGSR
110800000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
110900000000     C     MSGULT        IFNE      *ZEROS
111000000000     C                   EXSR      OUTMSG
111100000000     C                   ENDIF
111200000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
111300000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
111400000000     C     SFLEOF        IFEQ      0
111500000000     C                   MOVEL     '0'           *IN42
111600000000     C                   ELSE
111700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
111800000000     C                   MOVEL     '1'           *IN42
111900000000     C                   ENDIF
112000000000     C* EMISSIONE SUBFILE
112100000000     C     SFLULT        IFLE      0
112200000000     C*  --> CASO DI SFL VUOTO
112300000000     C                   MOVEL     '0'           *IN43
112400000000     C                   WRITE     EC75DVV
112500000000     C                   WRITE     EC75DVC
112600000000     C                   READ      EC75DVC                                99
112700000000     C                   ELSE
112800000000     C*  --> CASO DI SFL CON RECORDS
112900000000     C                   WRITE     EC75DVC
113000000000     C                   READ      EC75DVC                                99
113100000000     C                   ENDIF
113200000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
113300000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
113400000000     C                   Z-ADD     FRTSFL        VCCNBR
113500000000     C                   ENDSR
113600000000     C*------------------------------------------------------------------------*
113700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
113800000000     C*          CODA MESSAGGI DEL PGM
113900000000     C*------------------------------------------------------------------------*
114000000000     C     CARMSG        BEGSR
114100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
114200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
114300000000     C     MSGULT        IFEQ      0
114400000000     C                   MOVEL     '1'           *IN40
114500000000     C                   WRITE     EC75DVK
114600000000     C                   MOVEL     '0'           *IN40
114700000000     C                   ENDIF
114800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
114900000000     C                   MOVEL     $MSG          DSMSMS
115000000000     C                   CALL      'XMSG'        XMSGPA
115100000000     C     DSMSER        IFEQ      0
115200000000     C                   MOVEL     DSMSKE        VMCKEY
115300000000     C                   ENDIF
115400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
115500000000     C                   ADD       1             MSGULT            4 0
115600000000     C                   WRITE     EC75DVM
115700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
115800000000     C                   MOVEL     *BLANKS       $MSG
115900000000     C                   ENDSR
116000000000     C*------------------------------------------------------------------------*
116100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
116200000000     C*------------------------------------------------------------------------*
116300000000     C     OUTMSG        BEGSR
116400000000     C* VISUALIZZAZIONE MESSAGGIO
116500000000     C                   MOVEL     '1'           *IN44
116600000000     C                   WRITE     EC75DVK
116700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
116800000000     C                   Z-ADD     *ZEROS        MSGULT
116900000000     C                   ENDSR
117000000000     C*------------------------------------------------------------------------*
117100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
117200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
117300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
117400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
117500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
117600000000     C*------------------------------------------------------------------------*
117700000000     C     CARFUN        BEGSR
117800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
117900000000     C     1             DO        31            I                 5 0
118000000000     C                   MOVEL     FUN(I)        FUNRIG
118100000000     C                   MOVEL     FUNFUN        FFU(I)
118200000000     C                   MOVEL     FUNDES        FDE(I)
118300020923     C     DUTLPO        IFEQ      'S'
118400000000     C     $TGES         IFEQ      'G'
118500000000     C                   MOVEL     FUNASG        FAT(I)
118600000000     C                   ELSE
118700000000     C                   MOVEL     FUNASI        FAT(I)
118800000000     C                   ENDIF
118900000000     C                   ELSE
119000000000     C     $TGES         IFEQ      'G'
119100000000     C                   MOVEL     FUNAFG        FAT(I)
119200000000     C                   ELSE
119300000000     C                   MOVEL     FUNAFI        FAT(I)
119400000000     C                   ENDIF
119500000000     C                   ENDIF
119600000000     C     FAT(I)        IFEQ      'E'
119700000000     C                   MOVEL     USER          WEDP              3
119800000000     C     WEDP          IFEQ      'EDP'
119900000000     C                   MOVEL     'S'           FAT(I)
120000000000     C                   ELSE
120100000000     C                   MOVEL     'N'           FAT(I)
120200000000     C                   ENDIF
120300000000     C                   ENDIF
120400000000     C                   ENDDO
120500000000     C                   ENDSR
120600000000     C*------------------------------------------------------------------------*
120700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
120800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
120900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
121000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
121100000000     C*------------------------------------------------------------------------*
121200000000     C     OUTFUN        BEGSR
121300000000     C     1             DO        8             I
121400000000     C                   MOVEL     *BLANKS       FUV(I)
121500000000     C                   ENDDO
121600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
121700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
121800000000     C     1             DO        31            I
121900000000     C     FAT(I)        IFEQ      'S'
122000000000     C     FDE(I)        ANDNE     *BLANKS
122100000000     C                   ADD       1             FUNNAT
122200000000     C                   ENDIF
122300000000     C                   ENDDO
122400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
122500000000     C     FUNNAT        IFGT      8
122600000000     C                   MOVEL     'S'           FAT(24)
122700000000     C                   ENDIF
122800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
122900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
123000000000     C                   Z-ADD     8             FUNVIS            5 0
123100000000     C     FAT(23)       IFEQ      'S'
123200000000     C                   SUB       1             FUNVIS
123300000000     C                   ENDIF
123400000000     C     FAT(24)       IFEQ      'S'
123500000000     C                   SUB       1             FUNVIS
123600000000     C                   ENDIF
123700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
123800000000     C                   Z-ADD     *ZEROS        W                 5 0
123900000000     C                   ADD       1             FUNULT
124000000000     C     FUNULT        IFGT      31
124100000000     C                   Z-ADD     1             FUNULT
124200000000     C                   ENDIF
124300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
124400000000     C     FUNULT        DO        31            I
124500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
124600000000     C     W             IFLT      FUNVIS
124700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124800000000     C     FAT(I)        IFEQ      'S'
124900000000     C     FDE(I)        ANDNE     *BLANKS
125000000000     C     I             IFEQ      23
125100000000     C     I             OREQ      24
125200000000     C                   ELSE
125300000000     C                   ADD       1             W
125400000000     C                   MOVEL     FDE(I)        FUV(W)
125500000000     C                   ENDIF
125600000000     C                   ENDIF
125700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
125800000000     C                   Z-ADD     I             FUNULT
125900000000     C                   ENDIF
126000000000     C                   ENDDO
126100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
126200000000     C                   MOVEL     FUV(1)        VFD001
126300000000     C                   MOVEL     FUV(2)        VFD002
126400000000     C                   MOVEL     FUV(3)        VFD003
126500000000     C                   MOVEL     FUV(4)        VFD004
126600000000     C                   MOVEL     FUV(5)        VFD005
126700000000     C                   MOVEL     FUV(6)        VFD006
126800000000     C     FAT(23)       IFEQ      'N'
126900000000     C     FAT(24)       ANDEQ     'N'
127000000000     C                   MOVEL     FUV(7)        VFD007
127100000000     C                   MOVEL     FUV(8)        VFD008
127200000000     C                   ELSE
127300000000     C     FAT(23)       IFEQ      'S'
127400000000     C     FAT(24)       ANDEQ     'S'
127500000000     C                   MOVEL     FDE(23)       VFD007
127600000000     C                   MOVEL     FDE(24)       VFD008
127700000000     C                   ELSE
127800000000     C                   MOVEL     FUV(7)        VFD007
127900000000     C     FAT(23)       IFEQ      'S'
128000000000     C                   MOVEL     FDE(23)       VFD008
128100000000     C                   ELSE
128200000000     C                   MOVEL     FDE(24)       VFD008
128300000000     C                   ENDIF
128400000000     C                   ENDIF
128500000000     C                   ENDIF
128600000000     C                   MOVEL     '1'           $TESVD
128700000000     C                   ENDSR
128800000000     C*------------------------------------------------------------------------*
128900000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
129000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
129100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
129200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
129300000000     C*------------------------------------------------------------------------*
129400000000     C     CONFUN        BEGSR
129500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
129600000000     C     1             DO        31            I
129700000000     C     KEY           IFEQ      KEYS(I)
129800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
129900000000     C     FAT(I)        IFNE      'S'
130000000000     C                   MOVEL     MSG(1)        $MSG
130100000000     C                   EXSR      CARMSG
130200000000     C                   MOVEL     '1'           $ERR
130300000000     C                   ENDIF
130400000000     C                   ENDIF
130500000000     C                   ENDDO
130600000000     C                   ENDSR
130700000000     C*------------------------------------------------------------------------*
130800000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
130900000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
131000000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
131100000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
131200000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
131300000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
131400000000     C*------------------------------------------------------------------------*
131500000000     C     CAROPZ        BEGSR
131600000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
131700000000     C     1             DO        24            I
131800000000     C                   MOVEL     OPZ(I)        OPZRIG
131900000000     C                   MOVEL     OPZOPD        OPD(I)
132000000000     C                   MOVEL     OPZOPS        OPS(I)
132100000000     C                   MOVEL     OPZDES        ODE(I)
132200020923     C     DUTLPO        IFEQ      'S'
132300000000     C     $TGES         IFEQ      'G'
132400000000     C                   MOVEL     OPZASG        OAT(I)
132500000000     C                   ELSE
132600000000     C                   MOVEL     OPZASI        OAT(I)
132700000000     C                   ENDIF
132800000000     C                   ELSE
132900000000     C     $TGES         IFEQ      'G'
133000000000     C                   MOVEL     OPZAFG        OAT(I)
133100000000     C                   ELSE
133200000000     C                   MOVEL     OPZAFI        OAT(I)
133300000000     C                   ENDIF
133400000000     C                   ENDIF
133500000000     C     OAT(I)        IFEQ      'E'
133600000000     C                   MOVEL     USER          WEDP              3
133700000000     C     WEDP          IFEQ      'EDP'
133800000000     C                   MOVEL     'S'           OAT(I)
133900000000     C                   ELSE
134000000000     C                   MOVEL     'N'           OAT(I)
134100000000     C                   ENDIF
134200000000     C                   ENDIF
134300000000     C                   ENDDO
134400000000     C                   ENDSR
134500000000     C*------------------------------------------------------------------------*
134600000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
134700000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
134800000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
134900000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
135000000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
135100000000     C*------------------------------------------------------------------------*
135200000000     C     OUTOPZ        BEGSR
135300000000     C     1             DO        8             I
135400000000     C                   MOVEL     *BLANKS       OPV(I)
135500000000     C                   ENDDO
135600000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
135700000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
135800000000     C                   ADD       1             OPZULT
135900000000     C     OPZULT        IFGT      24
136000000000     C                   Z-ADD     1             OPZULT
136100000000     C                   END
136200000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
136300000000     C     1             DO        24            I
136400000000     C     OAT(I)        IFEQ      'S'
136500000000     C     ODE(I)        ANDNE     *BLANKS
136600000000     C                   ADD       1             OPZNAT
136700000000     C                   ENDIF
136800000000     C                   ENDDO
136900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
137000000000     C                   Z-ADD     0             W
137100000000     C     OPZULT        DO        24            I
137200000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
137300000000     C     W             IFLT      8
137400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
137500000000     C     OAT(I)        IFEQ      'S'
137600000000     C     ODE(I)        ANDNE     *BLANKS
137700000000     C                   ADD       1             W
137800000000     C                   MOVEL     ODE(I)        OPV(W)
137900000000     C                   ENDIF
138000000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
138100000000     C                   Z-ADD     I             OPZULT
138200000000     C                   ENDIF
138300000000     C                   ENDDO
138400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
138500000000     C                   MOVEL     OPV(1)        VOD001
138600000000     C                   MOVEL     OPV(2)        VOD002
138700000000     C                   MOVEL     OPV(3)        VOD003
138800000000     C                   MOVEL     OPV(4)        VOD004
138900000000     C                   MOVEL     OPV(5)        VOD005
139000000000     C                   MOVEL     OPV(6)        VOD006
139100000000     C                   MOVEL     OPV(7)        VOD007
139200000000     C                   MOVEL     OPV(8)        VOD008
139300000000     C                   MOVEL     '1'           $TESVD
139400000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
139500000000     C     OPZNAT        IFGT      8
139600000000     C                   MOVEL     'S'           FAT(23)
139700000000     C                   ENDIF
139800000000     C                   ENDSR
139900000000     C*------------------------------------------------------------------------*
140000000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
140100000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
140200000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
140300000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
140400000000     C*------------------------------------------------------------------------*
140500000000     C     CONOPZ        BEGSR
140600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
140700000000     C                   MOVEL     *BLANKS       OPZOK             1
140800000000     C     1             DO        24            I                 5 0
140900000000     C     VSCOPZ        IFEQ      OPD(I)
141000000000     C     VSCOPZ        OREQ      OPS(I)
141100000000     C     OAT(I)        IFEQ      'S'
141200000000     C                   MOVEL     OPD(I)        VSCOPZ
141300000000     C                   MOVEL     'S'           OPZOK
141400000000     C                   ELSE
141500000000     C                   MOVEL     'N'           OPZOK             1
141600000000     C                   ENDIF
141700000000     C                   ENDIF
141800000000     C                   ENDDO
141900000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
142000000000     C     OPZOK         IFNE      'S'
142100000000     C                   MOVEL     MSG(3)        $MSG
142200000000     C                   EXSR      CARMSG
142300000000     C                   MOVEL     '1'           $ERR
142400000000     C                   MOVEL     'N'           OPZOK             1
142500000000     C                   ENDIF
142600000000     C                   ENDSR
142700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
142800000000TREC75R- Tasto funzionale non valido.                                          1
142900000000TREC75R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
143000000000TREC75R- Opzione non valida.                                                   3
143100000000TREC75R- Opzione abilitata ma non gestita. Avvertire CED.                      4
143200000000TREC75R- Inizio elenco.                                                        5
143300000000TREC75R- Fine elenco.                                                          6
143400000000TREC75R- 7
143500000000TREC75R- 8
143600000000TREC75R- Immettere data formalmente valida                                     9
143700000000TREC75R- 10
143800000000TREC75R- 11
143900000000TREC75R- Codice competenza annullato, consentito solo il ripristino           12
144000000000TREC75R- Codice competenza non annullato, impossibile ripristinare            13
144100000000TREC75R- 14
144200000000TREC75R- 15
144300000000TREC75R- 16
144400000000TREC75R- 17
144500000000TREC75R- 18
144600000000TREC75R- 19
144700000000TREC75R- 20
144800000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
144900000000- 1-  -                   -    NN     NN
145000000000- 2-2 -2=Modifica         -    SN     NN
145100000000- 3-3 -                   -    NN     NN
145200000000- 4-4 -4=Cancellazione    -    SN     NN
145300000000- 5-5 -5=Visualizzazione  -    SS     SS
145400000000- 6-6 -                   -    NN     NN
145500000000- 7-7 -7=Ripristino       -    SN     NN
145600000000- 8-8 -                   -    NN     NN
145700000000- 9-9 -                   -    NN     NN
145800000000-10-  -                   -    NN     NN
145900000000-11-  -                   -    NN     NN
146000000000-12-  -                   -    NN     NN
146100000000-13-  -                   -    NN     NN
146200000000-14-  -                   -    NN     NN
146300000000-15-  -                   -    NN     NN
146400000000-16-  -                   -    NN     NN
146500000000-17-  -                   -    NN     NN
146600000000-18-  -                   -    NN     NN
146700000000-19-  -                   -    NN     NN
146800000000-20-  -                   -    NN     NN
146900000000-21-  -                   -    NN     NN
147000000000-22-  -                   -    NN     NN
147100000000-23-  -                   -    NN     NN
147200000000-24-  -                   -    NN     NN
147300000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
147400000000F1    -                   -    NN     NN
147500000000F2    -                   -    NN     NN
147600000000F3    -F3=Fine            -    SS     SS
147700000000F4    -                   -    NN     NN
147800000000F5    -F5=Rivisualizza    -    SS     SS
147900000000F6    -F6=Inserimento     -    SN     NN
148000000000F7    -                   -    NN     NN
148100000000F8    -                   -    NN     NN
148200000000F9    -                   -    NN     NN
148300000000F10   -                   -    NN     NN
148400000000F11   -                   -    NN     NN
148500000000F12   -F12=Ritorno        -    SS     SS
148600000000F13   -                   -    NN     NN
148700000000F14   -                   -    NN     NN
148800000000F15   -                   -    NN     NN
148900000000F16   -                   -    NN     NN
149000961003F17   -F17=Inizio dati    -    NN     NN
149100961003F18   -F18=Fine dati      -    NN     NN
149200000000F19   -                   -    NN     NN
149300000000F20   -                   -    NN     NN
149400000000F21   -                   -    NN     NN
149500000000F22   -                   -    NN     NN
149600000000F23   -F23=Altre opzioni  -    NN     NN
149700000000F24   -F24=Altri tasti    -    NN     NN
149800000000ENTER -                   -    SS     SS
149900000000ROLLUP-                   -    SS     SS
150000000000ROLLDN-                   -    SS     SS
150100000000HELP  -                   -    NN     NN
150200000000PRINT -                   -    NN     NN
150300000000BSPACE-                   -    NN     NN
150400000000CLEAR -                   -    NN     NN

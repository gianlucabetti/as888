000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC72R
000300000000      *  Descrizione   :  Gestione competenze di conto economico
000400000000      *                   Programma di visualizzazione subfile e scelta
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  26 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC72D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC72DVS:SFLULT)
001400000000     F                                     SFILE(EC72DVM:MSGULT)
001500000000     FECECT01L  IF   E           K DISK
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
004100031212     D*------------------
004200031212     D* DS GESTIONE RICERCA CONTROLLO TABELLE
004300031212     D*------------------
004400031212     D TIBS02DS      E DS
004500031212     D*------------------
004600031212     D* DS LETTURA TABELLA CO1 tipo data x calcolo
004700031212     D*------------------
004800031212     D DCO1          E DS
004900000000     D*---------------------
005000000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005100000000     D*---------------------
005200000000     D DSOPZ         E DS
005300000000     D  OPZRIG                 1     40
005400000000     D*---------------------
005500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005600000000     D*---------------------
005700000000     D DSFUN         E DS
005800000000     D  FUNRIG                 1     40
005900000000     D*---------------------
006000000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006100000000     D*---------------------
006200000000     D DSMSG         E DS
006300000000     D*---------------------
006400020923     D* DS REPERIMENTO DATI UTENTE
006500020923     D*-------------------
006501130919     D Trec69DS      E DS
006600020923     D TIBS34DS      E DS
006700020923     D DDATIUTE      E DS
006800020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006900000000     D*---------------------
007000000000     D* DS PER GESTIONE TASTI FUNZIONALI
007100000000     D*---------------------
007200000000     D DSKEYS        E DS
007300000000     D  KEYRIG                 1     40
007400000000     D*---------------------
007500000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007600000000     D*---------------------
007700000000     D                SDS
007800000000     D  USER                 254    263
007900000000     D*---------------------
008000000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008100000000     D*---------------------
008200000000     D DEVDS1          DS
008300000000     D* CHIAVE FUNZIONALE BATTUTA
008400000000     D  KEY                  369    369
008500000000     D* POSIZIONE CURSORE (IN HEX)
008600000000     D  CURSOR               370    371
008700000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008800000000     D  FRTSFL               378    379B 0
008900000000     D*---------------------
009000000000     D* DS ESTERNA DI PROCEDURA
009100000000     D*---------------------
009200000000     D DS60          E DS                  EXTNAME(TREC60DS)
009300000000     D*---------------------
009400000000     D* ARCHITETTURA
009500000000     D*---------------------
009600000000     D KPJBA         E DS
009601130919     d  savkpjbu       s                   like(kpjbu)
009700000000     D*------------------
009800000000     D* CALL XSRDA8 - CONTROLLO DATE
009900000000     D*------------------
010000000000     D WLBDAT          DS                  INZ
010100000000     D  G02DAT                 1      8  0
010200000000     D  G02INV                 9     16  0
010300000000     D  G02ERR                17     17
010400000000     D  G02TGI                18     22  0
010500000000     D*------------------
010600000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010700000000     D*------------------
010800000000     D DESGES          C                   CONST('** GESTIONE **')
010900000000     D DESINT          C                   CONST('INTERROGAZIONE')
011000000000     C*------------------------------------------------------------------------*
011100000000     C* MAIN LINE
011200000000     C*------------------------------------------------------------------------*
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000DO  1C     $FINE         DOUEQ     '1'
011600000000     C*
011700000000     C                   MOVEL     '0'           $ERR
011800000000     C*
011900000000     C* CONTROLLA GESTIONE VIDEO
012000000000IF  2C     $GES          IFEQ      'VS'
012100000000     C                   EXSR      GESVS
012200000000E   2C                   ENDIF
012300000000     C*
012400000000E   1C                   ENDDO
012500000000     C*
012600000000IF  1C     $F03          IFEQ      '1'
012700000000     C                   MOVEL     '1'           D60F03
012800000000E   1C                   ENDIF
012900000000     C* FINE PGM
013000000000     C                   MOVEL     DS60          KPJBU
013100000000     C                   SETON                                        LR
013200000000     C*------------------------------------------------------------------------*
013300000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
013400000000     C*------------------------------------------------------------------------*
013500000000     C     CARVS         BEGSR
013600000000     C*
013700000000     C                   MOVEL     'VS'          $GES
013800000000     C*
013900000000     C                   EXSR      CAROPZ
014000000000     C                   EXSR      OUTOPZ
014100000000     C                   MOVEA     FUN2          FUN
014200000000     C                   EXSR      CARFUN
014300000000     C                   EXSR      OUTFUN
014400000000     C*
014500000000     C                   WRITE     EC72DVT
014600000000     C                   WRITE     EC72DVO
014700000000     C                   WRITE     EC72DVF
014800000000     C*
014900000000     C                   EXSR      POSSFL
015000000000     C*
015100000000     C                   ENDSR
015200000000     C*------------------------------------------------------------------------*
015300000000     C* GESVS  - GESTIONE SUBFILE
015400000000     C*------------------------------------------------------------------------*
015500000000     C     GESVS         BEGSR
015600000000     C*
015700000000IF  1C     $TESVD        IFEQ      '1'
015800000000     C                   WRITE     EC72DVT
015900000000     C                   WRITE     EC72DVO
016000000000     C                   WRITE     EC72DVF
016100000000E   1C                   ENDIF
016200000000     C*
016300000000     C                   EXSR      OUTSFL
016400000000     C*
016500000000     C                   EXSR      CONFUN
016600000000     C*
016700000000IF  1C     $ERR          IFEQ      '0'
016800000000SL  2C                   SELECT
016900000000     C     KEY           WHENEQ    F3
017000000000     C                   EXSR      F03GES
017100000000     C     KEY           WHENEQ    F6
017200000000     C                   EXSR      F06GES
017300000000     C                   OTHER
017400000000     C                   EXSR      FUNSFL
017500000000E   2C                   ENDSL
017600000000E   1C                   ENDIF
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*------------------------------------------------------------------------*
018000000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> USCITA
018100000000     C*------------------------------------------------------------------------*
018200000000     C     F03GES        BEGSR
018300000000     C*
018400000000     C* SE SONO IN SCELTA (OPZIONE 1), QUINDI RICHIAMATO DA UN ALTRO PGM CON
018500000000     C* PUNTO INTERROGATIVO, USO F3 COME F12
018600000000     C     D60OP1        IFNE      'O01'
018700000000     C*
018800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
018900000000     C                   MOVEL     '1'           $F03
019000000000     C                   ENDIF
019100000000     C*
019200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
019300000000     C                   MOVEL     '1'           $FINE
019400000000     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019800000000     C*------------------------------------------------------------------------*
019900000000     C     F06GES        BEGSR
020000000000     C*
020100000000     C* LANCIO L'INSERIMENTO
020200000000     C                   Z-ADD     VCCNBC        WNBC
020300000000     C                   Z-ADD     *ZEROS        WCMP
020400000000     C                   Z-ADD     *ZEROS        WDDC
020500000000     C*
020600000000     C                   MOVEL     D60OP0        WOP0              3
020700000000     C                   MOVEL     'F06'         WOP1              3
020800000000     C                   MOVEL     'TREC73R'     WPGM              8
020900000000     C                   EXSR      LANPGM
021000940721     C*
021100940721     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
021200940721     C                   Z-ADD     D60NBC        VCCNBC
021300940722     C                   Z-ADD     1             SWMODI
021400940722     C*
021500940722     C* RICARICO IL SUBFILE
021600940722     C                   EXSR      POSSFL
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C*  GESOPZ - GESTIONE OPZIONI
022100000000     C*------------------------------------------------------------------------*
022200000000     C     GESOPZ        BEGSR
022300000000     C*
022400000000SL  1C                   SELECT
022500000000     C     VSCOPZ        WHENEQ    ' 1'
022600000000     C                   EXSR      O01GES
022700000000     C     VSCOPZ        WHENEQ    ' 2'
022800000000     C                   EXSR      O02GES
022900000000     C     VSCOPZ        WHENEQ    ' 4'
023000000000     C                   EXSR      O04GES
023100000000     C     VSCOPZ        WHENEQ    ' 5'
023200000000     C                   EXSR      O05GES
023300000000     C     VSCOPZ        WHENEQ    ' 7'
023400000000     C                   EXSR      O07GES
023500000000     C     VSCOPZ        WHENEQ    '11'
023600000000     C                   EXSR      O11GES
023700040209     C     VSCOPZ        WHENEQ    '12'
023800040209     C                   EXSR      O12GES
023801130919     C     VSCOPZ        WHENEQ    '17'
023802130919     C                   EXSR      O17GES
023900000000     C                   OTHER
024000000000     C                   MOVEL     MSG(4)        $MSG
024100000000     C                   EXSR      CARMSG
024200000000E   1C                   ENDSL
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600000000     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
024700000000     C*------------------------------------------------------------------------*
024800000000     C     O01GES        BEGSR
024900000000     C*
025000000000     C                   Z-ADD     VCCNBC        D60NBC
025100000000     C                   Z-ADD     VSCCMP        D60CMP
025200000000     C                   Z-ADD     VSCDDC        D60DDC
025300000000     C                   MOVE      '1'           $FINE
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
025800000000     C*------------------------------------------------------------------------*
025900000000     C     O02GES        BEGSR
026000000000     C*
026100000000IF  1C     VSCATB        IFEQ      'A'
026200000000     C                   MOVEL     '1'           $ERR
026300000000     C                   MOVEL     MSG(12)       $MSG
026400000000     C                   EXSR      CARMSG
026500000000E   1C                   ENDIF
026600000000     C*
026700000000IF  1C     $ERR          IFEQ      '0'
026800000000     C*
026900000000     C* LANCIO LA MODIFICA
027000000000     C                   Z-ADD     VCCNBC        WNBC
027100000000     C                   Z-ADD     VSCCMP        WCMP
027200000000     C                   Z-ADD     VSCDDC        WDDC
027300000000     C*
027400000000     C                   MOVEL     D60OP0        WOP0              3
027500000000     C                   MOVEL     'O02'         WOP1              3
027600000000     C                   MOVEL     'TREC73R'     WPGM              8
027700000000     C                   EXSR      LANPGM
027800000000     C*
027900000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
028000000000     C                   Z-ADD     D60NBC        VCCNBC
028100000000     C                   Z-ADD     1             SWMODI
028200000000     C*
028300000000E   1C                   ENDIF
028400000000     C*
028500000000     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
028800000000     C*------------------------------------------------------------------------*
028900000000     C     O04GES        BEGSR
029000000000     C*
029100000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
029200000000IF  1C     VSCATB        IFEQ      'A'
029300000000     C                   MOVEL     '1'           $ERR
029400000000     C                   MOVEL     MSG(12)       $MSG
029500000000     C                   EXSR      CARMSG
029600000000E   1C                   ENDIF
029700000000     C*
029800000000IF  1C     $ERR          IFEQ      '0'
029900000000     C*
030000000000     C* LANCIO LA CANCELLAZIONE
030100000000     C                   Z-ADD     VCCNBC        WNBC
030200000000     C                   Z-ADD     VSCCMP        WCMP
030300000000     C                   Z-ADD     VSCDDC        WDDC
030400000000     C*
030500000000     C                   MOVEL     D60OP0        WOP0              3
030600000000     C                   MOVEL     'O04'         WOP1              3
030700000000     C                   MOVEL     'TREC73R'     WPGM              8
030800000000     C                   EXSR      LANPGM
030900000000     C*
031000000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
031100000000     C                   Z-ADD     D60NBC        VCCNBC
031200000000     C                   Z-ADD     1             SWMODI
031300000000     C*
031400000000E   1C                   ENDIF
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031900000000     C*------------------------------------------------------------------------*
032000000000     C     O05GES        BEGSR
032100000000     C*
032200000000     C* LANCIO LA VISUALIZZAZIONE
032300000000     C                   Z-ADD     VCCNBC        WNBC
032400000000     C                   Z-ADD     VSCCMP        WCMP
032500000000     C                   Z-ADD     VSCDDC        WDDC
032600000000     C*
032700000000     C                   MOVEL     D60OP0        WOP0              3
032800000000     C                   MOVEL     'O05'         WOP1              3
032900000000     C                   MOVEL     'TREC73R'     WPGM              8
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
034800000000     C                   Z-ADD     VCCNBC        WNBC
034900000000     C                   Z-ADD     VSCCMP        WCMP
035000000000     C                   Z-ADD     VSCDDC        WDDC
035100000000     C*
035200000000     C                   MOVEL     D60OP0        WOP0              3
035300000000     C                   MOVEL     'O07'         WOP1              3
035400000000     C                   MOVEL     'TREC73R'     WPGM              8
035500000000     C                   EXSR      LANPGM
035600000000     C*
035700000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
035800000000     C                   Z-ADD     D60NBC        VCCNBC
035900000000     C                   Z-ADD     1             SWMODI
036000000000     C*
036100000000E   1C                   ENDIF
036200000000     C*
036300000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500000000     C*  O11GES - GESTIONE OPZIONE 11=GESTIONE CONTI DI CONTABILITA'
036600000000     C*------------------------------------------------------------------------*
036700000000     C     O11GES        BEGSR
036800000000     C*
036900000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
037000000000     C                   Z-ADD     VCCNBC        WNBC
037100000000     C                   Z-ADD     VSCCMP        WCMP
037200000000     C                   Z-ADD     VSCDDC        WDDC
037300940722     C                   MOVEL     00            WDDC
037400000000     C*
037500000000     C                   MOVEL     D60OP0        WOP0              3
037600000000     C                   MOVEL     'O11'         WOP1              3
037700000000     C                   MOVEL     'TREC75R'     WPGM              8
037800000000     C                   EXSR      LANPGM
037900000000     C*
038000000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
038100000000     C                   Z-ADD     D60NBC        VCCNBC
038200000000     C*
038300000000     C                   ENDSR
038400040209     C*------------------------------------------------------------------------*
038500040209     C*  O12GES - GESTIONE OPZIONE 12=voci
038600040209     C*------------------------------------------------------------------------*
038700040209     C     O12GES        BEGSR
038800040209     C*
038900040209     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
039000040209     C                   Z-ADD     VCCNBC        WNBC
039100040209     C                   Z-ADD     VSCCMP        WCMP
039200040209     C                   Z-ADD     VSCDDC        WDDC
039300040209     C                   MOVEL     01            WDDC
039400040209     C*
039500040209     C                   MOVEL     D60OP0        WOP0              3
039600040209     C                   MOVEL     'O12'         WOP1              3
039700040209     C                   MOVEL     'TREC60R'     WPGM              8
039800040209     C                   EXSR      LANPGM
039900040209     C*
040000040209     C                   ENDSR
040001130919     C*------------------------------------------------------------------------*
040002130919     C*  O17GES - GESTIONE OPZIONE 17=GESTIONE note conte economico
040003130919     C*------------------------------------------------------------------------*
040004130919     C     O17GES        BEGSR
040005130919     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
040006130919     c                   movel(p)  kpjbu         savkpjbu
040007130919     C                   MOVEL     'C'           d69tpana
040008130919     C                   Z-ADD     VSCcmp        d69voc
040009130919     C                   Z-ADD     Vccnbc        d69prg
040010130919     c                   movel(p)  trec69ds      kpjbu
040011130919     C                   call      'TREC69R'
040012130919     c                   parm                    kpjba
040013130919     c                   movel(p)  savkpjbu      kpjbu
040014130919     C*
040015130919     C                   ENDSR
040100000000     C*------------------------------------------------------------------------*
040200000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
040300000000     C*------------------------------------------------------------------------*
040400000000     C     NEWPOS        BEGSR
040500000000     C*
040600000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
040700000000     C*        1 = RICHIESTO POSIZIONAMENTO
040800000000     C                   Z-ADD     0             SWPOSI            7 0
040900000000     C*
041000000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
041100000000     C*            NON OCCORRE RICARICARE IL SUBFILE
041200000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
041300000000     C                   Z-ADD     0             SWMODI            1 0
041400000000     C*
041500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
041600000000IF  1C     VCCCMP        IFNE      DEPCMP
041700000000     C     VCCDDC        ORNE      DEPDDC
041800130911     C     VCCvod        ORNE      DEPvod
041801130917     C     VCCVAL        ORNE      DEPVAL
041900000000     C*
042000000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
042100000000     C                   EXSR      CHKPAR
042200000000     C*
042300000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
042400000000IF  2C     $ERR          IFEQ      '0'
042500000000     C                   Z-ADD     1             SWPOSI
042600000000     C                   Z-ADD     VCCCMP        DEPCMP
042700000000     C                   Z-ADD     VCCDDC        DEPDDC
042800130911     C                   movel     VCCvod        DEPvod
042801130917     C                   Z-ADD     VCCVAL        DEPVAL
042900000000E   2C                   ENDIF
043000000000E   1C                   ENDIF
043100000000     C*
043200000000     C                   ENDSR
043300000000     C*------------------------------------------------------------------------*
043400000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
043500000000     C*------------------------------------------------------------------------*
043600000000     C     CHKPAR        BEGSR
043700000000     C*
043800000000     C* REIMPOSTO INDICATORI DI ERRORE
043900130918     C                   SETOFF                                       8183
044000000000     C*--------------------
044100000000     C* DATA DECORRENZA COMPETENZA
044200000000     C*--------------------
044300000000     C                   Z-ADD     *ZERO         DDCVCC
044400000000IF  1C     VCCDDC        IFGT      *ZEROS
044500000000     C                   Z-ADD     VCCDDC        G02DAT
044600000000     C                   MOVEL     01            G02DAT
044700000000     C                   Z-ADD     *ZEROS        G02INV
044800000000     C                   MOVEL     '0'           G02ERR
044900000000     C                   CALL      'XSRDA8'
045000000000     C                   PARM                    WLBDAT
045100000000     C*
045200000000IF  2C     G02ERR        IFEQ      '1'
045300000000     C                   MOVEL     '1'           $ERR
045400000000     C                   MOVEL     MSG(9)        $MSG
045500000000     C                   EXSR      CARMSG
045600000000     C                   SETON                                        81
045700000000X   2C                   ELSE
045800000000     C                   Z-ADD     G02DAT        VCCDDC
045900000000     C                   Z-ADD     G02INV        DDCVCC            8 0
046000000000     C                   MOVE      00            DDCVCC
046100000000E   2C                   ENDIF
046200000000E   1C                   ENDIF
046201130917     C*--------------------
046202130917     C* VALIDITA' AL
046203130917     C*--------------------
046204130917     C                   Z-ADD     *ZERO         VALVCC            8 0
046205130917IF  1C     VCCVAL        IFGT      *ZEROS
046206130917     C                   Z-ADD     VCCVAL        G02DAT
046207130917     C                   Z-ADD     *ZEROS        G02INV
046208130917     C                   MOVEL     '0'           G02ERR
046209130917     C                   CALL      'XSRDA8'
046210130917     C                   PARM                    WLBDAT
046211130917     C*
046212130917IF  2C     G02ERR        IFEQ      '1'
046213130917     C                   MOVEL     '1'           $ERR
046214130917     C                   MOVEL     MSG(10)       $MSG
046215130917     C                   EXSR      CARMSG
046216130917     C                   SETON                                        83
046217130917X   2C                   ELSE
046218130917     C                   Z-ADD     G02DAT        VCCVAL
046219130917     C                   Z-ADD     G02INV        VALVCC            8 0
046220130917E   2C                   ENDIF
046221130917E   1C                   ENDIF
046300000000     C*
046400000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
046500000000     C* SUL CAMPO DEL CONTROL
046600000000IF  1C     $ERR          IFEQ      '1'
046700000000     C                   MOVEL     '0'           *IN43
046800000000X   1C                   ELSE
046900000000     C                   MOVEL     '1'           *IN43
047000000000E   1C                   ENDIF
047100000000     C*
047200000000     C                   ENDSR
047300000000     C*------------------------------------------------------------------------*
047400000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
047500000000     C*------------------------------------------------------------------------*
047600000000     C     SETFIL        BEGSR
047700000000     C*
047800000000     C                   MOVEL     '1'           *IN43
047900000000     C*
048000000000     C* POSIZIONAMENTO ARCHIVIO
048100000000     C                   Z-ADD     VCCNBC        KCTNBC
048200000000     C                   Z-ADD     VCCCMP        KCTCMP
048300000000     C                   Z-ADD     DDCVCC        KCTDDC
048400000000     C     KEYECT        SETLL     ECECT01L                           99
048500000000     C*
048600000000     C  N99              EXSR      LETFIL
048700000000     C*
048800000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
048900000000IF  1C     *IN99         IFEQ      '1'
049000000000     C                   Z-ADD     1             SFLEOF
049100000000E   1C                   ENDIF
049200000000     C*
049300000000     C                   ENDSR
049400000000     C*------------------------------------------------------------------------*
049500000000     C* LETFIL - LEGGE RECORD SUCESSIVO
049600000000     C*------------------------------------------------------------------------*
049700000000     C     LETFIL        BEGSR
049800000000     C*
049900000000     C* LEGGO FINO A:
050000000000     C*     - FINE FILE
050100000000     C*     - TROVATO RECORD VALIDO
050200000000     C                   MOVEL     '0'           WRECOK            1
050300000000DO  1C     *IN99         DOWEQ     *OFF
050400000000     C     WRECOK        ANDEQ     '0'
050500000000     C*
050600000000     C* LETTURA
050700000000     C     VCCNBC        READE     ECECT01L                               99
050800000000     C*
050900000000     C* FINE FILE  O TABELLA DIVERSA
051000000000IF  2C     *IN99         IFEQ      '1'
051100000000     C                   Z-ADD     1             SFLEOF
051200000000X   2C                   ELSE
051300000000     C                   EXSR      CHKREC
051400000000E   2C                   ENDIF
051500000000     C*
051600000000E   1C                   ENDDO
051700000000     C*
051800000000     C                   ENDSR
051900000000     C*------------------------------------------------------------------------*
052000000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
052100000000     C*------------------------------------------------------------------------*
052200000000     C     CHKREC        BEGSR
052300000000     C*
052400000000     C                   MOVEL     '1'           WRECOK
052500000000     C*
052600000000     C* SELEZIONE PER  ====>  DATA DECORRENZA
052700000000IF  1C     DDCVCC        IFGT      *ZEROS
052800000000IF  2C     DDCVCC        IFNE      ECTDDC
052900000000     C                   MOVEL     '0'           WRECOK
053000000000E   2C                   ENDIF
053100000000E   1C                   ENDIF
053101130917     C* VERIFICO SE RICHIESTO LA DATA DI VALIDITA'
053102130917     C     VALVCC        IFGT      *ZEROS
053103130917     C     EcTDDc        IFLE      VALVCC
053104130917     C     EcTDSc        ANDGE     VALVCC
053105130917     C                   ELSE
053106130917     C                   MOVEL     '0'           WRECOK
053107130917     C                   ENDIF
053108130917     C                   ENDIF
053200130911     C* VERIFICO SE RICHIESTa LA descrizione
053300130911     c                   if        vccvod <> ' '
053400130911     c                   z-add     0             pos               2 0
053500130911     C                   eval      pos = %scan(%trim(vccvod): ectcmd)
053600130911     c                   if        pos =  0
053700130911     C                   MOVEL     '0'           WRECOK
053800130911     C                   ENDIF
053900130911     C                   ENDIF
054000000000     C*
054100000000     C                   ENDSR
054200000000     C*------------------------------------------------------------------------*
054300000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
054400000000     C*------------------------------------------------------------------------*
054500000000     C     IRESFL        BEGSR
054600000000     C*
054700000000IF  1C     ECTATB        IFNE      *BLANKS
054800000000     C                   MOVEL     'A'           VSCATB
054900000000X   1C                   ELSE
055000000000     C                   MOVEL     ' '           VSCATB
055100000000E   1C                   ENDIF
055200000000     C*
055300000000     C                   Z-ADD     ECTCMP        VSCCMP
055400000000     C                   MOVE      ECTCMD        VSCCMD
055500000000     C                   MOVE      ECTFCR        VSCFCR
055600031212     c* decodifico flag data riferimento calcolo
055700031212     c                   clear                   vsdfdr
055800031212     c* Per anagrafiche obsolete
055900031212     c                   if        ectdsc < 20040101
056000031212     c                   if        ectfdr = '0'
056100031212     c                   eval      vsdfdr = 'MM spedizione'
056200031212     c                   else
056300031212     c                   eval      vsdfdr = 'MM successivo'
056400031212     c                   end
056500031212     c                   else
056600031212     c* dal 2004 il flag della data è decodificato con la tabella CO1
056700031212     C                   CLEAR                   TIBS02DS
056800031212     C                   CLEAR                   DCO1
056900031212     C                   EVAL      T02TLA = 'L'
057000031212     C                   MOVEL     'C'           T02MOD
057100031212     C                   MOVEL(P)  ectfdr        T02KE1
057200031212     C                   MOVEL     'CO1'         T02COD
057300031212     C                   MOVEL     KNSIF         T02SIF
057400031212     C                   CALL      'TIBS02R'
057500031212     C                   PARM                    KPJBA
057600031212     C                   PARM                    TIBS02DS
057700031212     C                   IF        T02ERR = *BLANKS
057800031212     C                   MOVEL     T02UNI        DCO1
057900031212     C                   MOVEL(p)  §CO1DES       Vsdfdr
058000031212     C                   ENDIF
058100031212E   1C                   ENDIF
058200031212     c*
058300980312     C                   MOVEL     ECTRIC        VSCRIC
058400000000     C                   Z-ADD     ECTDDC        G02INV
058500000000     C                   Z-ADD     *ZERO         G02DAT
058600000000     C                   MOVEL     '3'           G02ERR
058700000000     C                   CALL      'XSRDA8'
058800000000     C                   PARM                    WLBDAT
058900031212     C                   Z-ADD     G02DAT        VSCDDC
059000031212     C                   Z-ADD     G02DAT        VS1DDC
059100000000     C                   Z-ADD     ECTDSC        G02INV
059200000000     C                   Z-ADD     *ZERO         G02DAT
059300000000     C                   MOVEL     '3'           G02ERR
059400000000     C                   CALL      'XSRDA8'
059500000000     C                   PARM                    WLBDAT
059600031212     C                   Z-ADD     G02DAT        VSCDSC
059700031212     C                   Z-ADD     G02DAT        VS1DSC
059800000000     C*
059900000000     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* LANPGM - LANCIA UN PROGRAMMA
060200000000     C*------------------------------------------------------------------------*
060300000000     C     LANPGM        BEGSR
060400000000     C*--------------------
060500000000     C* PARAMETRI INFORMATIVI
060600000000     C*--------------------
060700000000     C                   MOVEL     WOP0          D60OP0
060800000000     C                   MOVEL     WOP1          D60OP1
060900000000     C                   MOVEL     '0'           D60F03
061000000000     C                   MOVEL     '0'           D60ERR
061100000000     C                   MOVEL     *BLANKS       D60MSG
061200000000     C*--------------------
061300000000     C* PARAMETRI
061400000000     C*--------------------
061500000000     C                   Z-ADD     WNBC          D60NBC
061600000000     C                   Z-ADD     WCMP          D60CMP
061700000000     C                   Z-ADD     WDDC          D60DDC
061800000000     C* LANCIO
061900000000     C                   MOVEL     DS60          KPJBU
062000000000     C                   CALL      WPGM                                 99
062100000000     C                   PARM                    KPJBA
062200000000     C                   MOVEL     KPJBU         DS60
062300000000     C*
062400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
062500000000IF  1C     D60F03        IFEQ      '1'
062600000000     C                   MOVEL     '1'           $F03
062700000000     C                   MOVEL     '1'           $ERR
062800000000     C                   MOVEL     '1'           $FINE
062900000000     C*
063000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
063100000000X   1C                   ELSE
063200000000     C                   MOVEL     D60MSG        $MSG
063300000000     C                   EXSR      CARMSG
063400000000E   1C                   ENDIF
063500000000     C*
063600000000     C                   ENDSR
063700020923     C*-------------------------------------------------------------------------
063800020923     C* REPERISCE I DATI UTENTE
063900020923     C*-------------------------------------------------------------------------
064000020923     C     REPDATIUTE    BEGSR
064100020923     C*
064200020923     C* INIZIALIZZA VARIABILI DI WRK
064300020923     C                   CLEAR                   TIBS34DS
064400020923     C                   CLEAR                   AZUTEDS
064500020923     C                   CLEAR                   DDATIUTE
064600020923     C*
064700020923     C     *DTAARA       DEFINE    §azute        azuteds
064800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
064900020923     C                   IN(E)     *DTAARA
065000020923if  1C                   IF        %Error
065100020923     c                   EVAL      I34Tla = 'L'
065200020923     C                   CALL      'TIBS34R'
065300020923     C                   PARM                    Tibs34Ds
065400020923     C                   IN        *DTAARA
065500020923e   1C                   ENDIF
065600020923     C*
065700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
065800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
065900020923     C*
066000020923     C                   ENDSR
066100000000     C*------------------------------------------------------------------------*
066200000000     C*  *INZSR - ROUTINE INIZIALE
066300000000     C*------------------------------------------------------------------------*
066400000000     C     *INZSR        BEGSR
066500000000     C*
066600000000     C* RICEVIMENTO PARAMETRI
066700000000     C     *ENTRY        PLIST
066800000000     C                   PARM                    KPJBA
066900000000     C                   MOVEL     KPJBU         DS60
067000000000     C*
067100000000     C* DEFINIZIONE KLIST
067200000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
067300000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
067400000000     C     *LIKE         DEFINE    ECTDDC        KCTDDC
067500000000     C     *LIKE         DEFINE    ECTNBC        WNBC
067600000000     C     *LIKE         DEFINE    ECTCMP        WCMP
067700000000     C     *LIKE         DEFINE    ECTDDC        WDDC
067800000000     C*
067900000000     C* DEFINIZIONE DEPOSITI
068000000000     C     *LIKE         DEFINE    VCCNBC        DEPNBC
068100000000     C     *LIKE         DEFINE    VCCCMP        DEPCMP
068200000000     C     *LIKE         DEFINE    VCCDDC        DEPDDC
068300130911     C     *LIKE         DEFINE    VCCvod        DEPvod
068301130917     C     *LIKE         DEFINE    VCCVAL        DEPVAL
068400000000     C*
068500000000     C* CHIAVE LETTURA ECECT01L - COMPLETA
068600000000     C     KEYECT        KLIST
068700000000     C                   KFLD                    KCTNBC
068800000000     C                   KFLD                    KCTCMP
068900000000     C                   KFLD                    KCTDDC
069000000000     C*
069100000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
069200000000     C                   MOVE      D60OP0        INTGES            2
069300000000IF  1C     INTGES        IFEQ      '02'
069400000000     C     D60OP1        ANDNE     'O01'
069500000000     C                   MOVEL     'G'           $TGES             1
069600000000     C                   MOVEL     DESGES        VTDOPE
069700000000X   1C                   ELSE
069800000000     C                   MOVEL     'I'           $TGES             1
069900000000     C                   MOVEL     DESINT        VTDOPE
070000000000E   1C                   ENDIF
070100000000     C*
070200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
070300000000     C                   EXSR      INZSTD
070400000000     C*
070500000000     C* SE RICHIAMATO PER LA SCELTA CON IL SFL (OPZIONE 1)
070600000000     C* INIBISCO LE ALTRE OPZIONI
070700000000IF  1C     D60OP1        IFEQ      'O01'
070800000000     C                   MOVEL     OPZ(5)        FUNRIG
070900000000     C                   MOVEL     'N'           FUNASG
071000000000     C                   MOVEL     'N'           FUNASI
071100000000     C                   MOVEL     'N'           FUNAFG
071200000000     C                   MOVEL     'N'           FUNAFG
071300000000     C                   MOVEL     FUNRIG        OPZ(5)
071400000000     C                   MOVEL     OPZ(11)       FUNRIG
071500000000     C                   MOVEL     'N'           FUNASG
071600000000     C                   MOVEL     'N'           FUNASI
071700000000     C                   MOVEL     'N'           FUNAFG
071800000000     C                   MOVEL     'N'           FUNAFG
071900000000     C                   MOVEL     FUNRIG        OPZ(11)
072000040209     C                   MOVEL     OPZ(12)       FUNRIG
072100040209     C                   MOVEL     'N'           FUNASG
072200040209     C                   MOVEL     'N'           FUNASI
072300040209     C                   MOVEL     'N'           FUNAFG
072400040209     C                   MOVEL     'N'           FUNAFG
072500040209     C                   MOVEL     FUNRIG        OPZ(12)
072600000000X   1C                   ELSE
072700000000     C                   MOVEL     OPZ(1)        FUNRIG
072800000000     C                   MOVEL     'N'           FUNASG
072900000000     C                   MOVEL     'N'           FUNASI
073000000000     C                   MOVEL     'N'           FUNAFG
073100000000     C                   MOVEL     'N'           FUNAFG
073200000000     C                   MOVEL     FUNRIG        OPZ(1)
073300000000E   1C                   ENDIF
073400000000     C*
073500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
073600000000     C                   EXSR      INZSFL
073700000000     C*
073800000000     C* IMPOSTO CAMPI TESTATA VIDEO
073900000000     C                   MOVEL     DSFIRS        VTDRSU
074000000000     C*
074100000000     C* DEVO REPERIRE IL NUMERO BLOCCO, L'ULTIMO IN ARCHIVIO
074200940721     C     *HIVAL        SETGT     ECECT01L
074300000000     C                   READP     ECECT01L                               99
074400000000     C   99              Z-ADD     1             VCCNBC
074500000000     C  N99              Z-ADD     ECTNBC        VCCNBC
074600000000     C*
074700000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
074800000000     C                   Z-ADD     *ZEROS        VCCCMP
074900000000     C                   Z-ADD     *ZEROS        VCCDDC
074901130917     C                   Z-ADD     *ZEROS        VCCVAL
075000130911     c                   clear                   vccvod
075100040209     c* se ricevuti parametri li imposto
075200040209     c                   if        d60cmp <> 0
075300040209     c                   z-add     d60cmp        vcccmp
075400040209     c                   end
075500000000     C*
075600000000     C* EMETTO VIDEATA SUBFILE
075700000000     C                   EXSR      CARVS
075800000000     C*
075900000000     C                   ENDSR
076000000000     C*------------------------------------------------------------------------*
076100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
076400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
076500000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
076600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076800000000     C*------------------------------------------------------------------------*
076900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
077000000000     C*------------------------------------------------------------------------*
077100000000     C     INZSTD        BEGSR
077200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
077300000000     C                   CALL      'XKEYS'
077400000000     C                   PARM                    DSKEYS
077500000000     C                   MOVEA     KEYRIG        KEYS
077600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
077700000000     C     XMSGPA        PLIST
077800000000     C                   PARM                    DSMSMS
077900000000     C                   PARM                    DSMSPG
078000000000     C                   PARM                    DSMSKE
078100000000     C                   PARM                    DSMSER
078200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
078300000000     C                   MOVEL     'TREC72R'     DSMSPG
078400000000     C                   MOVEL     'TREC72R'     VMCQUE
078500000000     C                   MOVEL     *BLANKS       VMCKEY
078600000000     C                   MOVEL     *BLANKS       DSMSMS
078700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
078800000000     C                   MOVEL     '0'           $ERR              1
078900000000     C                   MOVEL     '0'           $F03              1
079000000000     C                   MOVEL     *BLANKS       $GES              2
079100000000     C                   MOVEL     *BLANKS       $MSG             80
079200000000     C                   MOVEL     '0'           $FINE             1
079300000000     C                   MOVEL     '0'           $TESVD            1
079400000000     C* LANCIO ROUTINE PER VERIFICARE SE
079500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
079600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
079700020923     C                   EXSR      REPDATIUTE
079800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
079900000000     C                   BITOFF    '01234567'    POSMI1            1
080000000000     C                   BITOFF    '01234567'    POSMI2            1
080100000000     C                   BITOFF    '01234567'    POSMA1            1
080200000000     C                   BITOFF    '01234567'    POSMA2            1
080300000000     C                   BITON     '34'          POSMI1            1
080400000000     C                   BITON     '7'           POSMI2            1
080500000000     C                   BITON     '34'          POSMA1            1
080600000000     C                   BITON     '0'           POSMA2            1
080700000000     C                   MOVE      POSMI2        POSMIN            2
080800000000     C                   MOVEL     POSMI1        POSMIN
080900000000     C                   MOVE      POSMA2        POSMAX            2
081000000000     C                   MOVEL     POSMA1        POSMAX
081100000000     C                   ENDSR
081200000000     C*------------------------------------------------------------------------*
081300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
081400000000     C*------------------------------------------------------------------------*
081500000000     C     FUNSTD        BEGSR
081600000000     C                   SELECT
081700000000     C     KEY           WHENEQ    F3
081800000000     C                   EXSR      F03STD
081900000000     C     KEY           WHENEQ    F12
082000000000     C                   EXSR      F12STD
082100000000     C     KEY           WHENEQ    F24
082200000000     C                   EXSR      F24STD
082300000000     C                   OTHER
082400000000     C                   MOVEL     MSG(2)        $MSG
082500000000     C                   EXSR      CARMSG
082600000000     C                   ENDSL
082700000000     C                   ENDSR
082800000000     C*------------------------------------------------------------------------*
082900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
083000000000     C*------------------------------------------------------------------------*
083100000000     C     F03STD        BEGSR
083200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
083300000000     C                   MOVEL     '1'           $F03
083400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
083500000000     C                   MOVEL     '1'           $FINE
083600000000     C                   ENDSR
083700000000     C*------------------------------------------------------------------------*
083800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
083900000000     C*------------------------------------------------------------------------*
084000000000     C     F12STD        BEGSR
084100000000     C* IMPOSTO FLAG DI FIN PGM
084200000000     C                   MOVEL     '1'           $FINE
084300000000     C                   ENDSR
084400000000     C*------------------------------------------------------------------------*
084500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
084600000000     C*------------------------------------------------------------------------*
084700000000     C     F24STD        BEGSR
084800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
084900000000     C                   EXSR      OUTFUN
085000000000     C                   ENDSR
085100000000     C*------------------------------------------------------------------------*
085200000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
085300000000     C*------------------------------------------------------------------------*
085400000000     C     INZSFL        BEGSR
085500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
085600000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
085700000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
085800000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
085900000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
086000000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
086100000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
086200000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
086300000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
086400000000     C                   Z-ADD     12            SFLPAG
086500000000     C                   ENDSR
086600000000     C*------------------------------------------------------------------------*
086700000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
086800000000     C*------------------------------------------------------------------------*
086900000000     C     FUNSFL        BEGSR
087000000000     C                   SELECT
087100000000     C     KEY           WHENEQ    ENTER
087200000000     C                   EXSR      ENTSFL
087300000000     C     KEY           WHENEQ    F3
087400000000     C                   EXSR      F03STD
087500000000     C     KEY           WHENEQ    F5
087600000000     C                   EXSR      F05STD
087700000000     C     KEY           WHENEQ    F12
087800000000     C                   EXSR      F12STD
087900000000     C     KEY           WHENEQ    F17
088000000000     C                   EXSR      F17STD
088100000000     C     KEY           WHENEQ    F18
088200000000     C                   EXSR      F18STD
088300000000     C     KEY           WHENEQ    F23
088400000000     C                   EXSR      F23STD
088500000000     C     KEY           WHENEQ    F24
088600000000     C                   EXSR      F24STD
088700000000     C     KEY           WHENEQ    ROLLUP
088800000000     C                   EXSR      RUPSTD
088900000000     C     KEY           WHENEQ    ROLLDN
089000000000     C                   EXSR      RDNSTD
089100000000     C                   OTHER
089200000000     C                   MOVEL     MSG(2)        $MSG
089300000000     C                   EXSR      CARMSG
089400000000     C                   ENDSL
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
089800000000     C*------------------------------------------------------------------------*
089900000000     C     F05STD        BEGSR
090000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
090100000000     C                   EXSR      POSSFL
090200000000     C                   ENDSR
090300000000     C*------------------------------------------------------------------------*
090400000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
090500000000     C*------------------------------------------------------------------------*
090600000000     C     F17STD        BEGSR
090700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
090800000000     C                   Z-ADD     1             VCCNBR
090900000000     C                   ENDSR
091000000000     C*------------------------------------------------------------------------*
091100000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
091200000000     C*------------------------------------------------------------------------*
091300000000     C     F18STD        BEGSR
091400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
091500000000     C                   Z-ADD     SFLULT        VCCNBR
091600000000     C                   ENDSR
091700000000     C*------------------------------------------------------------------------*
091800000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
091900000000     C*------------------------------------------------------------------------*
092000000000     C     F23STD        BEGSR
092100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
092200000000     C                   EXSR      OUTOPZ
092300000000     C                   ENDSR
092400000000     C*------------------------------------------------------------------------*
092500000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
092600000000     C*------------------------------------------------------------------------*
092700000000     C     RUPSTD        BEGSR
092800000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
092900000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
093000000000     C     CURSOR        IFGE      POSMIN
093100000000     C     CURSOR        ANDLE     POSMAX
093200000000     C                   ELSE
093300000000     C                   EXSR      CARSFL
093400000000     C                   ENDIF
093500000000     C                   ENDSR
093600000000     C*------------------------------------------------------------------------*
093700000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
093800000000     C*------------------------------------------------------------------------*
093900000000     C     RDNSTD        BEGSR
094000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
094100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
094200000000     C     CURSOR        IFGE      POSMIN
094300000000     C     CURSOR        ANDLE     POSMAX
094400000000     C                   ELSE
094500000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
094600000000     C                   MOVEL     '1'           $ERR
094700000000     C                   MOVEL     MSG(5)        $MSG
094800000000     C                   EXSR      CARMSG
094900000000     C                   ENDIF
095000000000     C                   ENDSR
095100000000     C*------------------------------------------------------------------------*
095200000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
095300000000     C*------------------------------------------------------------------------*
095400000000     C     ENTSFL        BEGSR
095500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
095600000000     C                   EXSR      NEWPOS
095700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
095800000000     C     $ERR          IFEQ      '0'
095900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
096000000000     C     SWPOSI        IFNE      0
096100000000     C                   EXSR      POSSFL
096200000000     C                   ELSE
096300000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
096400000000     C     SFLULT        IFNE      0
096500000000     C                   EXSR      OPZSFL
096600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
096700000000     C     SWMODI        IFNE      0
096800000000     C                   EXSR      POSSFL
096900000000     C                   ENDIF
097000000000     C                   ENDIF
097100000000     C                   ENDIF
097200000000     C                   ENDIF
097300000000     C                   ENDSR
097400000000     C*------------------------------------------------------------------------*
097500000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
097600000000     C*------------------------------------------------------------------------*
097700000000     C     POSSFL        BEGSR
097800000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
097900000000     C                   EXSR      CLRSFL
098000000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
098100000000     C                   EXSR      SETFIL
098200000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
098300000000     C     SFLEOF        IFEQ      0
098400000000     C                   EXSR      CARSFL
098500000000     C                   ENDIF
098600000000     C                   ENDSR
098700000000     C*------------------------------------------------------------------------*
098800000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
098900000000     C*------------------------------------------------------------------------*
099000000000     C     OPZSFL        BEGSR
099100000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
099200000000     C                   MOVEL     'S'           OPZOK             1
099300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
099400000000     C                   Z-ADD     0             SFLVAR            5 0
099500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
099600000000     C                   Z-ADD     0             SFLERR
099700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
099800000000     C                   Z-ADD     SFLULT        WFLULT
099900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
100000000000     C                   READC     EC72DVS                                49
100100000000     C     *IN49         DOWEQ     '0'
100200941121     C     $ERR          ANDEQ     '0'
100300000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
100400000000     C                   ADD       1             SFLVAR
100500000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
100600000000     C     OPZOK         IFEQ      'S'
100700000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
100800000000     C     VSCOPZ        IFNE      *BLANKS
100900000000     C                   EXSR      CONOPZ
101000000000     C     OPZOK         IFEQ      'S'
101100941121     C     $ERR          ANDEQ     '0'
101200000000     C                   EXSR      GESOPZ
101300000000     C                   ENDIF
101400000000     C                   ENDIF
101500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
101600000000     C*            2. PULIZIA SCELTA IN SFL
101700000000     C     OPZOK         IFEQ      'S'
101800000000     C                   MOVEL     '0'           *IN41
101900000000     C                   MOVEL     *BLANKS       VSCOPZ
102000000000     C                   ELSE
102100000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
102200000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
102300000000     C                   MOVEL     '1'           *IN41
102400000000     C                   Z-ADD     SFLULT        SFLERR
102500000000     C                   ENDIF
102600000000     C                   ELSE
102700000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
102800000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
102900000000     C                   MOVEL     '1'           *IN41
103000000000     C                   ENDIF
103100000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
103200000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
103300000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
103400000000     C     OPZOK         IFNE      'S'
103500000000     C                   Z-ADD     SFLERR        VCCNBR
103600000000     C                   ELSE
103700000000     C                   Z-ADD     SFLULT        VCCNBR
103800000000     C                   ENDIF
103900000000     C* AGGIORNO RECORD SFL
104000000000     C                   UPDATE    EC72DVS
104100941121     C     $ERR          IFEQ      '0'
104200000000     C                   READC     EC72DVS                                49
104300941121     C                   ENDIF
104400000000     C                   ENDDO
104500000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
104600000000     C                   Z-ADD     WFLULT        SFLULT
104700000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104800000000     C     SFLVAR        IFEQ      0
104900000000     C                   MOVEL     '1'           $FINE
105000000000     C                   ENDIF
105100000000     C                   ENDSR
105200000000     C*------------------------------------------------------------------------*
105300000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
105400000000     C*------------------------------------------------------------------------*
105500000000     C     CLRSFL        BEGSR
105600000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
105700000000     C* (0=NON FINE FILE  1=FINE FILE)
105800000000     C                   Z-ADD     0             SFLEOF
105900000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
106000000000     C                   Z-ADD     0             SFLULT
106100000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
106200000000     C                   MOVEL     '1'           *IN40
106300000000     C                   WRITE     EC72DVC
106400000000     C                   MOVEL     '0'           *IN40
106500000000     C                   ENDSR
106600000000     C*------------------------------------------------------------------------*
106700000000     C* CARSFL - CARICAMENTO SFL
106800000000     C*------------------------------------------------------------------------*
106900000000     C     CARSFL        BEGSR
107000000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
107100000000     C     SFLULT        IFEQ      0
107200000000     C                   EXSR      SETFIL
107300000000     C                   ENDIF
107400000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
107500000000     C                   Z-ADD     SFLULT        WFLULT
107600000000     C* CICLO:
107700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
107800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
107900000000     C                   Z-ADD     1             SFLRIG
108000000000     C     SFLRIG        DOWLE     SFLPAG
108100000000     C     SFLEOF        ANDEQ     0
108200000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
108300000000     C                   MOVEL     *BLANKS       VSCOPZ
108400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
108500000000     C                   EXSR      IRESFL
108600000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
108700000000     C                   ADD       1             SFLULT
108800000000     C* SCRITTURA RECORD SFL
108900000000     C                   WRITE     EC72DVS
109000000000     C                   ADD       1             SFLRIG
109100000000     C* LETTURA FILE DA CARICARE
109200000000     C                   EXSR      LETFIL
109300000000     C                   ENDDO
109400000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
109500000000     C     WFLULT        IFNE      SFLULT
109600000000     C                   Z-ADD     WFLULT        VCCNBR
109700000000     C                   ADD       1             VCCNBR
109800000000     C                   ENDIF
109900000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
110000000000     C     SFLEOF        IFEQ      1
110100000000     C     SFLULT        ANDGT     *ZEROS
110200000000     C                   MOVEL     '1'           $ERR
110300000000     C                   MOVEL     MSG(6)        $MSG
110400000000     C                   EXSR      CARMSG
110500000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
110600000000     C     SFLULT        CHAIN     EC72DVS                            99
110700000000     C     *IN99         IFEQ      '0'
110800000000     C                   SETON                                        45
110900000000     C                   UPDATE    EC72DVS
111000000000     C                   SETOFF                                       45
111100000000     C                   ENDIF
111200000000     C                   ENDIF
111300000000     C                   ENDSR
111400000000     C*------------------------------------------------------------------------*
111500000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
111600000000     C*------------------------------------------------------------------------*
111700000000     C     OUTSFL        BEGSR
111800000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
111900000000     C     MSGULT        IFNE      *ZEROS
112000000000     C                   EXSR      OUTMSG
112100000000     C                   ENDIF
112200000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
112300000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
112400000000     C     SFLEOF        IFEQ      0
112500000000     C                   MOVEL     '0'           *IN42
112600000000     C                   ELSE
112700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
112800000000     C                   MOVEL     '1'           *IN42
112900000000     C                   ENDIF
113000000000     C* EMISSIONE SUBFILE
113100000000     C     SFLULT        IFLE      0
113200000000     C*  --> CASO DI SFL VUOTO
113300000000     C                   MOVEL     '0'           *IN43
113400000000     C                   WRITE     EC72DVV
113500000000     C                   WRITE     EC72DVC
113600000000     C                   READ      EC72DVC                                99
113700000000     C                   ELSE
113800000000     C*  --> CASO DI SFL CON RECORDS
113900000000     C                   WRITE     EC72DVC
114000000000     C                   READ      EC72DVC                                99
114100000000     C                   ENDIF
114200000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
114300000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
114400000000     C                   Z-ADD     FRTSFL        VCCNBR
114500000000     C                   ENDSR
114600000000     C*------------------------------------------------------------------------*
114700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
114800000000     C*          CODA MESSAGGI DEL PGM
114900000000     C*------------------------------------------------------------------------*
115000000000     C     CARMSG        BEGSR
115100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
115200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
115300000000     C     MSGULT        IFEQ      0
115400000000     C                   MOVEL     '1'           *IN40
115500000000     C                   WRITE     EC72DVK
115600000000     C                   MOVEL     '0'           *IN40
115700000000     C                   ENDIF
115800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
115900000000     C                   MOVEL     $MSG          DSMSMS
116000000000     C                   CALL      'XMSG'        XMSGPA
116100000000     C     DSMSER        IFEQ      0
116200000000     C                   MOVEL     DSMSKE        VMCKEY
116300000000     C                   ENDIF
116400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
116500000000     C                   ADD       1             MSGULT            4 0
116600000000     C                   WRITE     EC72DVM
116700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
116800000000     C                   MOVEL     *BLANKS       $MSG
116900000000     C                   ENDSR
117000000000     C*------------------------------------------------------------------------*
117100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
117200000000     C*------------------------------------------------------------------------*
117300000000     C     OUTMSG        BEGSR
117400000000     C* VISUALIZZAZIONE MESSAGGIO
117500000000     C                   MOVEL     '1'           *IN44
117600000000     C                   WRITE     EC72DVK
117700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
117800000000     C                   Z-ADD     *ZEROS        MSGULT
117900000000     C                   ENDSR
118000000000     C*------------------------------------------------------------------------*
118100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
118200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
118300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
118400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
118500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
118600000000     C*------------------------------------------------------------------------*
118700000000     C     CARFUN        BEGSR
118800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
118900000000     C     1             DO        31            I                 5 0
119000000000     C                   MOVEL     FUN(I)        FUNRIG
119100000000     C                   MOVEL     FUNFUN        FFU(I)
119200000000     C                   MOVEL     FUNDES        FDE(I)
119300020923     C     DUTLPO        IFEQ      'S'
119400000000     C     $TGES         IFEQ      'G'
119500000000     C                   MOVEL     FUNASG        FAT(I)
119600000000     C                   ELSE
119700000000     C                   MOVEL     FUNASI        FAT(I)
119800000000     C                   ENDIF
119900000000     C                   ELSE
120000000000     C     $TGES         IFEQ      'G'
120100000000     C                   MOVEL     FUNAFG        FAT(I)
120200000000     C                   ELSE
120300000000     C                   MOVEL     FUNAFI        FAT(I)
120400000000     C                   ENDIF
120500000000     C                   ENDIF
120600000000     C     FAT(I)        IFEQ      'E'
120700000000     C                   MOVEL     USER          WEDP              3
120800000000     C     WEDP          IFEQ      'EDP'
120900000000     C                   MOVEL     'S'           FAT(I)
121000000000     C                   ELSE
121100000000     C                   MOVEL     'N'           FAT(I)
121200000000     C                   ENDIF
121300000000     C                   ENDIF
121400000000     C                   ENDDO
121500000000     C                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
121800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
121900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
122000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
122100000000     C*------------------------------------------------------------------------*
122200000000     C     OUTFUN        BEGSR
122300000000     C     1             DO        8             I
122400000000     C                   MOVEL     *BLANKS       FUV(I)
122500000000     C                   ENDDO
122600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
122700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
122800000000     C     1             DO        31            I
122900000000     C     FAT(I)        IFEQ      'S'
123000000000     C     FDE(I)        ANDNE     *BLANKS
123100000000     C                   ADD       1             FUNNAT
123200000000     C                   ENDIF
123300000000     C                   ENDDO
123400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
123500000000     C     FUNNAT        IFGT      8
123600000000     C                   MOVEL     'S'           FAT(24)
123700000000     C                   ENDIF
123800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
123900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
124000000000     C                   Z-ADD     8             FUNVIS            5 0
124100000000     C     FAT(23)       IFEQ      'S'
124200000000     C                   SUB       1             FUNVIS
124300000000     C                   ENDIF
124400000000     C     FAT(24)       IFEQ      'S'
124500000000     C                   SUB       1             FUNVIS
124600000000     C                   ENDIF
124700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
124800000000     C                   Z-ADD     *ZEROS        W                 5 0
124900000000     C                   ADD       1             FUNULT
125000000000     C     FUNULT        IFGT      31
125100000000     C                   Z-ADD     1             FUNULT
125200000000     C                   ENDIF
125300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
125400000000     C     FUNULT        DO        31            I
125500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
125600000000     C     W             IFLT      FUNVIS
125700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
125800000000     C     FAT(I)        IFEQ      'S'
125900000000     C     FDE(I)        ANDNE     *BLANKS
126000000000     C     I             IFEQ      23
126100000000     C     I             OREQ      24
126200000000     C                   ELSE
126300000000     C                   ADD       1             W
126400000000     C                   MOVEL     FDE(I)        FUV(W)
126500000000     C                   ENDIF
126600000000     C                   ENDIF
126700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
126800000000     C                   Z-ADD     I             FUNULT
126900000000     C                   ENDIF
127000000000     C                   ENDDO
127100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
127200000000     C                   MOVEL     FUV(1)        VFD001
127300000000     C                   MOVEL     FUV(2)        VFD002
127400000000     C                   MOVEL     FUV(3)        VFD003
127500000000     C                   MOVEL     FUV(4)        VFD004
127600000000     C                   MOVEL     FUV(5)        VFD005
127700000000     C                   MOVEL     FUV(6)        VFD006
127800000000     C     FAT(23)       IFEQ      'N'
127900000000     C     FAT(24)       ANDEQ     'N'
128000000000     C                   MOVEL     FUV(7)        VFD007
128100000000     C                   MOVEL     FUV(8)        VFD008
128200000000     C                   ELSE
128300000000     C     FAT(23)       IFEQ      'S'
128400000000     C     FAT(24)       ANDEQ     'S'
128500000000     C                   MOVEL     FDE(23)       VFD007
128600000000     C                   MOVEL     FDE(24)       VFD008
128700000000     C                   ELSE
128800000000     C                   MOVEL     FUV(7)        VFD007
128900000000     C     FAT(23)       IFEQ      'S'
129000000000     C                   MOVEL     FDE(23)       VFD008
129100000000     C                   ELSE
129200000000     C                   MOVEL     FDE(24)       VFD008
129300000000     C                   ENDIF
129400000000     C                   ENDIF
129500000000     C                   ENDIF
129600000000     C                   MOVEL     '1'           $TESVD
129700000000     C                   ENDSR
129800000000     C*------------------------------------------------------------------------*
129900000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
130000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
130100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
130200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
130300000000     C*------------------------------------------------------------------------*
130400000000     C     CONFUN        BEGSR
130500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
130600000000     C     1             DO        31            I
130700000000     C     KEY           IFEQ      KEYS(I)
130800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
130900000000     C     FAT(I)        IFNE      'S'
131000000000     C                   MOVEL     MSG(1)        $MSG
131100000000     C                   EXSR      CARMSG
131200000000     C                   MOVEL     '1'           $ERR
131300000000     C                   ENDIF
131400000000     C                   ENDIF
131500000000     C                   ENDDO
131600000000     C                   ENDSR
131700000000     C*------------------------------------------------------------------------*
131800000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
131900000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
132000000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
132100000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
132200000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
132300000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
132400000000     C*------------------------------------------------------------------------*
132500000000     C     CAROPZ        BEGSR
132600000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
132700000000     C     1             DO        24            I
132800000000     C                   MOVEL     OPZ(I)        OPZRIG
132900000000     C                   MOVEL     OPZOPD        OPD(I)
133000000000     C                   MOVEL     OPZOPS        OPS(I)
133100000000     C                   MOVEL     OPZDES        ODE(I)
133200020923     C     DUTLPO        IFEQ      'S'
133300000000     C     $TGES         IFEQ      'G'
133400000000     C                   MOVEL     OPZASG        OAT(I)
133500000000     C                   ELSE
133600000000     C                   MOVEL     OPZASI        OAT(I)
133700000000     C                   ENDIF
133800000000     C                   ELSE
133900000000     C     $TGES         IFEQ      'G'
134000000000     C                   MOVEL     OPZAFG        OAT(I)
134100000000     C                   ELSE
134200000000     C                   MOVEL     OPZAFI        OAT(I)
134300000000     C                   ENDIF
134400000000     C                   ENDIF
134500000000     C     OAT(I)        IFEQ      'E'
134600000000     C                   MOVEL     USER          WEDP              3
134700000000     C     WEDP          IFEQ      'EDP'
134800000000     C                   MOVEL     'S'           OAT(I)
134900000000     C                   ELSE
135000000000     C                   MOVEL     'N'           OAT(I)
135100000000     C                   ENDIF
135200000000     C                   ENDIF
135300000000     C                   ENDDO
135400000000     C                   ENDSR
135500000000     C*------------------------------------------------------------------------*
135600000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
135700000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
135800000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
135900000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
136000000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
136100000000     C*------------------------------------------------------------------------*
136200000000     C     OUTOPZ        BEGSR
136300000000     C     1             DO        8             I
136400000000     C                   MOVEL     *BLANKS       OPV(I)
136500000000     C                   ENDDO
136600000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
136700000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
136800000000     C                   ADD       1             OPZULT
136900000000     C     OPZULT        IFGT      24
137000000000     C                   Z-ADD     1             OPZULT
137100000000     C                   END
137200000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
137300000000     C     1             DO        24            I
137400000000     C     OAT(I)        IFEQ      'S'
137500000000     C     ODE(I)        ANDNE     *BLANKS
137600000000     C                   ADD       1             OPZNAT
137700000000     C                   ENDIF
137800000000     C                   ENDDO
137900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
138000000000     C                   Z-ADD     0             W
138100000000     C     OPZULT        DO        24            I
138200000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
138300000000     C     W             IFLT      8
138400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
138500000000     C     OAT(I)        IFEQ      'S'
138600000000     C     ODE(I)        ANDNE     *BLANKS
138700000000     C                   ADD       1             W
138800000000     C                   MOVEL     ODE(I)        OPV(W)
138900000000     C                   ENDIF
139000000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
139100000000     C                   Z-ADD     I             OPZULT
139200000000     C                   ENDIF
139300000000     C                   ENDDO
139400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
139500000000     C                   MOVEL     OPV(1)        VOD001
139600000000     C                   MOVEL     OPV(2)        VOD002
139700000000     C                   MOVEL     OPV(3)        VOD003
139800000000     C                   MOVEL     OPV(4)        VOD004
139900000000     C                   MOVEL     OPV(5)        VOD005
140000000000     C                   MOVEL     OPV(6)        VOD006
140100000000     C                   MOVEL     OPV(7)        VOD007
140200000000     C                   MOVEL     OPV(8)        VOD008
140300000000     C                   MOVEL     '1'           $TESVD
140400000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
140500000000     C     OPZNAT        IFGT      8
140600000000     C                   MOVEL     'S'           FAT(23)
140700000000     C                   ENDIF
140800000000     C                   ENDSR
140900000000     C*------------------------------------------------------------------------*
141000000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
141100000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
141200000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
141300000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
141400000000     C*------------------------------------------------------------------------*
141500000000     C     CONOPZ        BEGSR
141600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
141700000000     C                   MOVEL     *BLANKS       OPZOK             1
141800000000     C     1             DO        24            I                 5 0
141900000000     C     VSCOPZ        IFEQ      OPD(I)
142000000000     C     VSCOPZ        OREQ      OPS(I)
142100000000     C     OAT(I)        IFEQ      'S'
142200000000     C                   MOVEL     OPD(I)        VSCOPZ
142300000000     C                   MOVEL     'S'           OPZOK
142400000000     C                   ELSE
142500000000     C                   MOVEL     'N'           OPZOK             1
142600000000     C                   ENDIF
142700000000     C                   ENDIF
142800000000     C                   ENDDO
142900000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
143000000000     C     OPZOK         IFNE      'S'
143100000000     C                   MOVEL     MSG(3)        $MSG
143200000000     C                   EXSR      CARMSG
143300000000     C                   MOVEL     '1'           $ERR
143400000000     C                   MOVEL     'N'           OPZOK             1
143500000000     C                   ENDIF
143600000000     C                   ENDSR
143700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
143800000000TREC72R- Tasto funzionale non valido.                                          1
143900000000TREC72R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
144000000000TREC72R- Opzione non valida.                                                   3
144100000000TREC72R- Opzione abilitata ma non gestita. Avvertire CED.                      4
144200000000TREC72R- Inizio elenco.                                                        5
144300000000TREC72R- Fine elenco.                                                          6
144400000000TREC72R- 7
144500000000TREC72R- 8
144600000000TREC72R- Immettere data formalmente valida                                     9
144700130917TREC72R- Data di validità non valida                                          10
144800000000TREC72R- 11
144900000000TREC72R- Codice competenza annullato, consentito solo il ripristino           12
145000000000TREC72R- Codice competenza non annullato, impossibile ripristinare            13
145100000000TREC72R- 14
145200000000TREC72R- 15
145300000000TREC72R- 16
145400000000TREC72R- 17
145500000000TREC72R- 18
145600000000TREC72R- 19
145700000000TREC72R- 20
145800000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
145900000000- 1-1 -1=Scelta           -    SS     SS
146000000000- 2-2 -2=Modifica         -    SN     NN
146100000000- 3-3 -                   -    NN     NN
146200000000- 4-4 -4=Cancellazione    -    SN     NN
146300000000- 5-5 -5=Visualizzazione  -    SS     SS
146400000000- 6-6 -                   -    NN     NN
146500000000- 7-7 -7=Ripristino       -    SN     NN
146600000000- 8-8 -                   -    NN     NN
146700000000- 9-9 -                   -    NN     NN
146800000000-10-  -                   -    NN     NN
146900000000-11-11-11=Conti contab.tà -    SS     SS
147000040209-12-12-12=Voci            -    SS     SS
147100000000-13-  -                   -    NN     NN
147200000000-14-  -                   -    NN     NN
147300000000-15-  -                   -    NN     NN
147400000000-16-  -                   -    NN     NN
147500130919-17-17-17=Note C.E.       -    SS     SS
147600000000-18-  -                   -    NN     NN
147700000000-19-  -                   -    NN     NN
147800000000-20-  -                   -    NN     NN
147900000000-21-  -                   -    NN     NN
148000000000-22-  -                   -    NN     NN
148100000000-23-  -                   -    NN     NN
148200000000-24-  -                   -    NN     NN
148300000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
148400000000F1    -                   -    NN     NN
148500000000F2    -                   -    NN     NN
148600000000F3    -F3=Fine            -    SS     SS
148700000000F4    -                   -    NN     NN
148800000000F5    -F5=Rivisualizza    -    SS     SS
148900000000F6    -F6=Inserimento     -    SN     NN
149000000000F7    -                   -    NN     NN
149100000000F8    -                   -    NN     NN
149200000000F9    -                   -    NN     NN
149300000000F10   -                   -    NN     NN
149400000000F11   -                   -    NN     NN
149500000000F12   -F12=Ritorno        -    SS     SS
149600000000F13   -                   -    NN     NN
149700000000F14   -                   -    NN     NN
149800000000F15   -                   -    NN     NN
149900000000F16   -                   -    NN     NN
150000961003F17   -F17=Inizio dati    -    NN     NN
150100961003F18   -F18=Fine dati      -    NN     NN
150200000000F19   -                   -    NN     NN
150300000000F20   -                   -    NN     NN
150400000000F21   -                   -    NN     NN
150500000000F22   -                   -    NN     NN
150600130919F23   -F23=Altre opzioni  -    SS     SS
150700000000F24   -F24=Altri tasti    -    NN     NN
150800000000ENTER -                   -    SS     SS
150900000000ROLLUP-                   -    SS     SS
151000000000ROLLDN-                   -    SS     SS
151100000000HELP  -                   -    NN     NN
151200000000PRINT -                   -    NN     NN
151300000000BSPACE-                   -    NN     NN
151400000000CLEAR -                   -    NN     NN

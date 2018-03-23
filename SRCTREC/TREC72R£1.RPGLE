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
004000000000     D*------------------------------------------------------------------------*
004100000000     D* INPUT
004200000000     D*------------------------------------------------------------------------*
004201031212     D*------------------
004202031212     D* DS GESTIONE RICERCA CONTROLLO TABELLE
004203031212     D*------------------
004204031212     D TIBS02DS      E DS
004205031212     D*------------------
004206031212     D* DS LETTURA TABELLA CO1 tipo data x calcolo
004207031212     D*------------------
004208031212     D DCO1          E DS
004300000000     D*---------------------
004400000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004500000000     D*---------------------
004600000000     D DSOPZ         E DS
004700000000     D  OPZRIG                 1     40
004800000000     D*---------------------
004900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005000000000     D*---------------------
005100000000     D DSFUN         E DS
005200000000     D  FUNRIG                 1     40
005300000000     D*---------------------
005400000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005500000000     D*---------------------
005600000000     D DSMSG         E DS
005700000000     D*---------------------
005800020923     D* DS REPERIMENTO DATI UTENTE
005900020923     D*-------------------
006000020923     D TIBS34DS      E DS
006100020923     D DDATIUTE      E DS
006200020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006300000000     D*---------------------
006400000000     D* DS PER GESTIONE TASTI FUNZIONALI
006500000000     D*---------------------
006600000000     D DSKEYS        E DS
006700000000     D  KEYRIG                 1     40
006800000000     D*---------------------
006900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007000000000     D*---------------------
007100000000     D                SDS
007200000000     D  USER                 254    263
007300000000     D*---------------------
007400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007500000000     D*---------------------
007600000000     D DEVDS1          DS
007700000000     D* CHIAVE FUNZIONALE BATTUTA
007800000000     D  KEY                  369    369
007900000000     D* POSIZIONE CURSORE (IN HEX)
008000000000     D  CURSOR               370    371
008100000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008200000000     D  FRTSFL               378    379B 0
008300000000     D*---------------------
008400000000     D* DS ESTERNA DI PROCEDURA
008500000000     D*---------------------
008600000000     D DS60          E DS                  EXTNAME(TREC60DS)
008700000000     D*---------------------
008800000000     D* ARCHITETTURA
008900000000     D*---------------------
009000000000     D KPJBA         E DS
009100000000     D*------------------
009200000000     D* CALL XSRDA8 - CONTROLLO DATE
009300000000     D*------------------
009400000000     D WLBDAT          DS                  INZ
009500000000     D  G02DAT                 1      8  0
009600000000     D  G02INV                 9     16  0
009700000000     D  G02ERR                17     17
009800000000     D  G02TGI                18     22  0
009900000000     D*------------------
010000000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010100000000     D*------------------
010200000000     D DESGES          C                   CONST('** GESTIONE **')
010300000000     D DESINT          C                   CONST('INTERROGAZIONE')
010400000000     C*------------------------------------------------------------------------*
010500000000     C* MAIN LINE
010600000000     C*------------------------------------------------------------------------*
010700000000     C*
010800000000     C* CICLO FINO A CHE:
010900000000DO  1C     $FINE         DOUEQ     '1'
011000000000     C*
011100000000     C                   MOVEL     '0'           $ERR
011200000000     C*
011300000000     C* CONTROLLA GESTIONE VIDEO
011400000000IF  2C     $GES          IFEQ      'VS'
011500000000     C                   EXSR      GESVS
011600000000E   2C                   ENDIF
011700000000     C*
011800000000E   1C                   ENDDO
011900000000     C*
012000000000IF  1C     $F03          IFEQ      '1'
012100000000     C                   MOVEL     '1'           D60F03
012200000000E   1C                   ENDIF
012300000000     C* FINE PGM
012400000000     C                   MOVEL     DS60          KPJBU
012500000000     C                   SETON                                        LR
012600000000     C*------------------------------------------------------------------------*
012700000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012800000000     C*------------------------------------------------------------------------*
012900000000     C     CARVS         BEGSR
013000000000     C*
013100000000     C                   MOVEL     'VS'          $GES
013200000000     C*
013300000000     C                   EXSR      CAROPZ
013400000000     C                   EXSR      OUTOPZ
013500000000     C                   MOVEA     FUN2          FUN
013600000000     C                   EXSR      CARFUN
013700000000     C                   EXSR      OUTFUN
013800000000     C*
013900000000     C                   WRITE     EC72DVT
014000000000     C                   WRITE     EC72DVO
014100000000     C                   WRITE     EC72DVF
014200000000     C*
014300000000     C                   EXSR      POSSFL
014400000000     C*
014500000000     C                   ENDSR
014600000000     C*------------------------------------------------------------------------*
014700000000     C* GESVS  - GESTIONE SUBFILE
014800000000     C*------------------------------------------------------------------------*
014900000000     C     GESVS         BEGSR
015000000000     C*
015100000000IF  1C     $TESVD        IFEQ      '1'
015200000000     C                   WRITE     EC72DVT
015300000000     C                   WRITE     EC72DVO
015400000000     C                   WRITE     EC72DVF
015500000000E   1C                   ENDIF
015600000000     C*
015700000000     C                   EXSR      OUTSFL
015800000000     C*
015900000000     C                   EXSR      CONFUN
016000000000     C*
016100000000IF  1C     $ERR          IFEQ      '0'
016200000000SL  2C                   SELECT
016300000000     C     KEY           WHENEQ    F3
016400000000     C                   EXSR      F03GES
016500000000     C     KEY           WHENEQ    F6
016600000000     C                   EXSR      F06GES
016700000000     C                   OTHER
016800000000     C                   EXSR      FUNSFL
016900000000E   2C                   ENDSL
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> USCITA
017500000000     C*------------------------------------------------------------------------*
017600000000     C     F03GES        BEGSR
017700000000     C*
017800000000     C* SE SONO IN SCELTA (OPZIONE 1), QUINDI RICHIAMATO DA UN ALTRO PGM CON
017900000000     C* PUNTO INTERROGATIVO, USO F3 COME F12
018000000000     C     D60OP1        IFNE      'O01'
018100000000     C*
018200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
018300000000     C                   MOVEL     '1'           $F03
018400000000     C                   ENDIF
018500000000     C*
018600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
018700000000     C                   MOVEL     '1'           $FINE
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
019200000000     C*------------------------------------------------------------------------*
019300000000     C     F06GES        BEGSR
019400000000     C*
019500000000     C* LANCIO L'INSERIMENTO
019600000000     C                   Z-ADD     VCCNBC        WNBC
019700000000     C                   Z-ADD     *ZEROS        WCMP
019800000000     C                   Z-ADD     *ZEROS        WDDC
019900000000     C*
020000000000     C                   MOVEL     D60OP0        WOP0              3
020100000000     C                   MOVEL     'F06'         WOP1              3
020200000000     C                   MOVEL     'TREC73R'     WPGM              8
020300000000     C                   EXSR      LANPGM
020400940721     C*
020500940721     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
020600940721     C                   Z-ADD     D60NBC        VCCNBC
020700940722     C                   Z-ADD     1             SWMODI
020800940722     C*
020900940722     C* RICARICO IL SUBFILE
021000940722     C                   EXSR      POSSFL
021100000000     C*
021200000000     C                   ENDSR
021300000000     C*------------------------------------------------------------------------*
021400000000     C*  GESOPZ - GESTIONE OPZIONI
021500000000     C*------------------------------------------------------------------------*
021600000000     C     GESOPZ        BEGSR
021700000000     C*
021800000000SL  1C                   SELECT
021900000000     C     VSCOPZ        WHENEQ    ' 1'
022000000000     C                   EXSR      O01GES
022100000000     C     VSCOPZ        WHENEQ    ' 2'
022200000000     C                   EXSR      O02GES
022300000000     C     VSCOPZ        WHENEQ    ' 4'
022400000000     C                   EXSR      O04GES
022500000000     C     VSCOPZ        WHENEQ    ' 5'
022600000000     C                   EXSR      O05GES
022700000000     C     VSCOPZ        WHENEQ    ' 7'
022800000000     C                   EXSR      O07GES
022900000000     C     VSCOPZ        WHENEQ    '11'
023000000000     C                   EXSR      O11GES
023001040209     C     VSCOPZ        WHENEQ    '12'
023002040209     C                   EXSR      O12GES
023100000000     C                   OTHER
023200000000     C                   MOVEL     MSG(4)        $MSG
023300000000     C                   EXSR      CARMSG
023400000000E   1C                   ENDSL
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
023900000000     C*------------------------------------------------------------------------*
024000000000     C     O01GES        BEGSR
024100000000     C*
024200000000     C                   Z-ADD     VCCNBC        D60NBC
024300000000     C                   Z-ADD     VSCCMP        D60CMP
024400000000     C                   Z-ADD     VSCDDC        D60DDC
024500000000     C                   MOVE      '1'           $FINE
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
025000000000     C*------------------------------------------------------------------------*
025100000000     C     O02GES        BEGSR
025200000000     C*
025300000000IF  1C     VSCATB        IFEQ      'A'
025400000000     C                   MOVEL     '1'           $ERR
025500000000     C                   MOVEL     MSG(12)       $MSG
025600000000     C                   EXSR      CARMSG
025700000000E   1C                   ENDIF
025800000000     C*
025900000000IF  1C     $ERR          IFEQ      '0'
026000000000     C*
026100000000     C* LANCIO LA MODIFICA
026200000000     C                   Z-ADD     VCCNBC        WNBC
026300000000     C                   Z-ADD     VSCCMP        WCMP
026400000000     C                   Z-ADD     VSCDDC        WDDC
026500000000     C*
026600000000     C                   MOVEL     D60OP0        WOP0              3
026700000000     C                   MOVEL     'O02'         WOP1              3
026800000000     C                   MOVEL     'TREC73R'     WPGM              8
026900000000     C                   EXSR      LANPGM
027000000000     C*
027100000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
027200000000     C                   Z-ADD     D60NBC        VCCNBC
027300000000     C                   Z-ADD     1             SWMODI
027400000000     C*
027500000000E   1C                   ENDIF
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*------------------------------------------------------------------------*
027900000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
028000000000     C*------------------------------------------------------------------------*
028100000000     C     O04GES        BEGSR
028200000000     C*
028300000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
028400000000IF  1C     VSCATB        IFEQ      'A'
028500000000     C                   MOVEL     '1'           $ERR
028600000000     C                   MOVEL     MSG(12)       $MSG
028700000000     C                   EXSR      CARMSG
028800000000E   1C                   ENDIF
028900000000     C*
029000000000IF  1C     $ERR          IFEQ      '0'
029100000000     C*
029200000000     C* LANCIO LA CANCELLAZIONE
029300000000     C                   Z-ADD     VCCNBC        WNBC
029400000000     C                   Z-ADD     VSCCMP        WCMP
029500000000     C                   Z-ADD     VSCDDC        WDDC
029600000000     C*
029700000000     C                   MOVEL     D60OP0        WOP0              3
029800000000     C                   MOVEL     'O04'         WOP1              3
029900000000     C                   MOVEL     'TREC73R'     WPGM              8
030000000000     C                   EXSR      LANPGM
030100000000     C*
030200000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
030300000000     C                   Z-ADD     D60NBC        VCCNBC
030400000000     C                   Z-ADD     1             SWMODI
030500000000     C*
030600000000E   1C                   ENDIF
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031100000000     C*------------------------------------------------------------------------*
031200000000     C     O05GES        BEGSR
031300000000     C*
031400000000     C* LANCIO LA VISUALIZZAZIONE
031500000000     C                   Z-ADD     VCCNBC        WNBC
031600000000     C                   Z-ADD     VSCCMP        WCMP
031700000000     C                   Z-ADD     VSCDDC        WDDC
031800000000     C*
031900000000     C                   MOVEL     D60OP0        WOP0              3
032000000000     C                   MOVEL     'O05'         WOP1              3
032100000000     C                   MOVEL     'TREC73R'     WPGM              8
032200000000     C                   EXSR      LANPGM
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
032700000000     C*------------------------------------------------------------------------*
032800000000     C     O07GES        BEGSR
032900000000     C*
033000000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033100000000IF  1C     VSCATB        IFNE      'A'
033200000000     C                   MOVEL     '1'           $ERR
033300000000     C                   MOVEL     MSG(13)       $MSG
033400000000     C                   EXSR      CARMSG
033500000000E   1C                   ENDIF
033600000000     C*
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C*
033900000000     C* LANCIO IL RIPRISTINO
034000000000     C                   Z-ADD     VCCNBC        WNBC
034100000000     C                   Z-ADD     VSCCMP        WCMP
034200000000     C                   Z-ADD     VSCDDC        WDDC
034300000000     C*
034400000000     C                   MOVEL     D60OP0        WOP0              3
034500000000     C                   MOVEL     'O07'         WOP1              3
034600000000     C                   MOVEL     'TREC73R'     WPGM              8
034700000000     C                   EXSR      LANPGM
034800000000     C*
034900000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
035000000000     C                   Z-ADD     D60NBC        VCCNBC
035100000000     C                   Z-ADD     1             SWMODI
035200000000     C*
035300000000E   1C                   ENDIF
035400000000     C*
035500000000     C                   ENDSR
035600000000     C*------------------------------------------------------------------------*
035700000000     C*  O11GES - GESTIONE OPZIONE 11=GESTIONE CONTI DI CONTABILITA'
035800000000     C*------------------------------------------------------------------------*
035900000000     C     O11GES        BEGSR
036000000000     C*
036100000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
036200000000     C                   Z-ADD     VCCNBC        WNBC
036300000000     C                   Z-ADD     VSCCMP        WCMP
036400000000     C                   Z-ADD     VSCDDC        WDDC
036500940722     C                   MOVEL     00            WDDC
036600000000     C*
036700000000     C                   MOVEL     D60OP0        WOP0              3
036800000000     C                   MOVEL     'O11'         WOP1              3
036900000000     C                   MOVEL     'TREC75R'     WPGM              8
037000000000     C                   EXSR      LANPGM
037100000000     C*
037200000000     C* IMPOSTO IL NUOVO NR. BLOCCO, SE C'E'
037300000000     C                   Z-ADD     D60NBC        VCCNBC
037400000000     C*
037500000000     C                   ENDSR
037501040209     C*------------------------------------------------------------------------*
037502040209     C*  O12GES - GESTIONE OPZIONE 12=voci
037503040209     C*------------------------------------------------------------------------*
037504040209     C     O12GES        BEGSR
037505040209     C*
037506040209     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
037507040209     C                   Z-ADD     VCCNBC        WNBC
037508040209     C                   Z-ADD     VSCCMP        WCMP
037509040209     C                   Z-ADD     VSCDDC        WDDC
037510040209     C                   MOVEL     01            WDDC
037511040209     C*
037512040209     C                   MOVEL     D60OP0        WOP0              3
037513040209     C                   MOVEL     'O12'         WOP1              3
037514040209     C                   MOVEL     'TREC60R'     WPGM              8
037515040209     C                   EXSR      LANPGM
037519040209     C*
037520040209     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
037800000000     C*------------------------------------------------------------------------*
037900000000     C     NEWPOS        BEGSR
038000000000     C*
038100000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
038200000000     C*        1 = RICHIESTO POSIZIONAMENTO
038300000000     C                   Z-ADD     0             SWPOSI            7 0
038400000000     C*
038500000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
038600000000     C*            NON OCCORRE RICARICARE IL SUBFILE
038700000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038800000000     C                   Z-ADD     0             SWMODI            1 0
038900000000     C*
039000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
039100000000IF  1C     VCCCMP        IFNE      DEPCMP
039200000000     C     VCCDDC        ORNE      DEPDDC
039300000000     C*
039400000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
039500000000     C                   EXSR      CHKPAR
039600000000     C*
039700000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
039800000000IF  2C     $ERR          IFEQ      '0'
039900000000     C                   Z-ADD     1             SWPOSI
040000000000     C                   Z-ADD     VCCCMP        DEPCMP
040100000000     C                   Z-ADD     VCCDDC        DEPDDC
040200000000E   2C                   ENDIF
040300000000E   1C                   ENDIF
040400000000     C*
040500000000     C                   ENDSR
040600000000     C*------------------------------------------------------------------------*
040700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
040800000000     C*------------------------------------------------------------------------*
040900000000     C     CHKPAR        BEGSR
041000000000     C*
041100000000     C* REIMPOSTO INDICATORI DI ERRORE
041200000000     C                   SETOFF                                         81
041300000000     C*--------------------
041400000000     C* DATA DECORRENZA COMPETENZA
041500000000     C*--------------------
041600000000     C                   Z-ADD     *ZERO         DDCVCC
041700000000IF  1C     VCCDDC        IFGT      *ZEROS
041800000000     C                   Z-ADD     VCCDDC        G02DAT
041900000000     C                   MOVEL     01            G02DAT
042000000000     C                   Z-ADD     *ZEROS        G02INV
042100000000     C                   MOVEL     '0'           G02ERR
042200000000     C                   CALL      'XSRDA8'
042300000000     C                   PARM                    WLBDAT
042400000000     C*
042500000000IF  2C     G02ERR        IFEQ      '1'
042600000000     C                   MOVEL     '1'           $ERR
042700000000     C                   MOVEL     MSG(9)        $MSG
042800000000     C                   EXSR      CARMSG
042900000000     C                   SETON                                        81
043000000000X   2C                   ELSE
043100000000     C                   Z-ADD     G02DAT        VCCDDC
043200000000     C                   Z-ADD     G02INV        DDCVCC            8 0
043300000000     C                   MOVE      00            DDCVCC
043400000000E   2C                   ENDIF
043500000000E   1C                   ENDIF
043600000000     C*
043700000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
043800000000     C* SUL CAMPO DEL CONTROL
043900000000IF  1C     $ERR          IFEQ      '1'
044000000000     C                   MOVEL     '0'           *IN43
044100000000X   1C                   ELSE
044200000000     C                   MOVEL     '1'           *IN43
044300000000E   1C                   ENDIF
044400000000     C*
044500000000     C                   ENDSR
044600000000     C*------------------------------------------------------------------------*
044700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
044800000000     C*------------------------------------------------------------------------*
044900000000     C     SETFIL        BEGSR
045000000000     C*
045100000000     C                   MOVEL     '1'           *IN43
045200000000     C*
045300000000     C* POSIZIONAMENTO ARCHIVIO
045400000000     C                   Z-ADD     VCCNBC        KCTNBC
045500000000     C                   Z-ADD     VCCCMP        KCTCMP
045600000000     C                   Z-ADD     DDCVCC        KCTDDC
045700000000     C     KEYECT        SETLL     ECECT01L                           99
045800000000     C*
045900000000     C  N99              EXSR      LETFIL
046000000000     C*
046100000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
046200000000IF  1C     *IN99         IFEQ      '1'
046300000000     C                   Z-ADD     1             SFLEOF
046400000000E   1C                   ENDIF
046500000000     C*
046600000000     C                   ENDSR
046700000000     C*------------------------------------------------------------------------*
046800000000     C* LETFIL - LEGGE RECORD SUCESSIVO
046900000000     C*------------------------------------------------------------------------*
047000000000     C     LETFIL        BEGSR
047100000000     C*
047200000000     C* LEGGO FINO A:
047300000000     C*     - FINE FILE
047400000000     C*     - TROVATO RECORD VALIDO
047500000000     C                   MOVEL     '0'           WRECOK            1
047600000000DO  1C     *IN99         DOWEQ     *OFF
047700000000     C     WRECOK        ANDEQ     '0'
047800000000     C*
047900000000     C* LETTURA
048000000000     C     VCCNBC        READE     ECECT01L                               99
048100000000     C*
048200000000     C* FINE FILE  O TABELLA DIVERSA
048300000000IF  2C     *IN99         IFEQ      '1'
048400000000     C                   Z-ADD     1             SFLEOF
048500000000X   2C                   ELSE
048600000000     C                   EXSR      CHKREC
048700000000E   2C                   ENDIF
048800000000     C*
048900000000E   1C                   ENDDO
049000000000     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
049400000000     C*------------------------------------------------------------------------*
049500000000     C     CHKREC        BEGSR
049600000000     C*
049700000000     C                   MOVEL     '1'           WRECOK
049800000000     C*
049900000000     C* SELEZIONE PER  ====>  DATA DECORRENZA
050000000000IF  1C     DDCVCC        IFGT      *ZEROS
050100000000IF  2C     DDCVCC        IFNE      ECTDDC
050200000000     C                   MOVEL     '0'           WRECOK
050300000000E   2C                   ENDIF
050400000000E   1C                   ENDIF
050500000000     C*
050600000000     C                   ENDSR
050700000000     C*------------------------------------------------------------------------*
050800000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
050900000000     C*------------------------------------------------------------------------*
051000000000     C     IRESFL        BEGSR
051100000000     C*
051200000000IF  1C     ECTATB        IFNE      *BLANKS
051300000000     C                   MOVEL     'A'           VSCATB
051400000000X   1C                   ELSE
051500000000     C                   MOVEL     ' '           VSCATB
051600000000E   1C                   ENDIF
051700000000     C*
051800000000     C                   Z-ADD     ECTCMP        VSCCMP
051900000000     C                   MOVE      ECTCMD        VSCCMD
052000000000     C                   MOVE      ECTFCR        VSCFCR
052001031212     c* decodifico flag data riferimento calcolo
052002031212     c                   clear                   vsdfdr
052101031212     c* Per anagrafiche obsolete
052103031212     c                   if        ectdsc < 20040101
052105031212     c                   if        ectfdr = '0'
052106031212     c                   eval      vsdfdr = 'MM spedizione'
052107031212     c                   else
052108031212     c                   eval      vsdfdr = 'MM successivo'
052109031212     c                   end
052116031212     c                   else
052117031212     c* dal 2004 il flag della data è decodificato con la tabella CO1
052118031212     C                   CLEAR                   TIBS02DS
052119031212     C                   CLEAR                   DCO1
052120031212     C                   EVAL      T02TLA = 'L'
052127031212     C                   MOVEL     'C'           T02MOD
052128031212     C                   MOVEL(P)  ectfdr        T02KE1
052130031212     C                   MOVEL     'CO1'         T02COD
052131031212     C                   MOVEL     KNSIF         T02SIF
052132031212     C                   CALL      'TIBS02R'
052133031212     C                   PARM                    KPJBA
052134031212     C                   PARM                    TIBS02DS
052137031212     C                   IF        T02ERR = *BLANKS
052139031212     C                   MOVEL     T02UNI        DCO1
052140031212     C                   MOVEL(p)  §CO1DES       Vsdfdr
052146031212     C                   ENDIF
052149031212E   1C                   ENDIF
052150031212     c*
052200980312     C                   MOVEL     ECTRIC        VSCRIC
052300000000     C                   Z-ADD     ECTDDC        G02INV
052400000000     C                   Z-ADD     *ZERO         G02DAT
052500000000     C                   MOVEL     '3'           G02ERR
052600000000     C                   CALL      'XSRDA8'
052700000000     C                   PARM                    WLBDAT
052900031212     C                   Z-ADD     G02DAT        VSCDDC
052901031212     C                   Z-ADD     G02DAT        VS1DDC
053200000000     C                   Z-ADD     ECTDSC        G02INV
053300000000     C                   Z-ADD     *ZERO         G02DAT
053400000000     C                   MOVEL     '3'           G02ERR
053500000000     C                   CALL      'XSRDA8'
053600000000     C                   PARM                    WLBDAT
053800031212     C                   Z-ADD     G02DAT        VSCDSC
053801031212     C                   Z-ADD     G02DAT        VS1DSC
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* LANPGM - LANCIA UN PROGRAMMA
054500000000     C*------------------------------------------------------------------------*
054600000000     C     LANPGM        BEGSR
054700000000     C*--------------------
054800000000     C* PARAMETRI INFORMATIVI
054900000000     C*--------------------
055000000000     C                   MOVEL     WOP0          D60OP0
055100000000     C                   MOVEL     WOP1          D60OP1
055200000000     C                   MOVEL     '0'           D60F03
055300000000     C                   MOVEL     '0'           D60ERR
055400000000     C                   MOVEL     *BLANKS       D60MSG
055500000000     C*--------------------
055600000000     C* PARAMETRI
055700000000     C*--------------------
055800000000     C                   Z-ADD     WNBC          D60NBC
055900000000     C                   Z-ADD     WCMP          D60CMP
056000000000     C                   Z-ADD     WDDC          D60DDC
056100000000     C* LANCIO
056200000000     C                   MOVEL     DS60          KPJBU
056300000000     C                   CALL      WPGM                                 99
056400000000     C                   PARM                    KPJBA
056500000000     C                   MOVEL     KPJBU         DS60
056600000000     C*
056700000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
056800000000IF  1C     D60F03        IFEQ      '1'
056900000000     C                   MOVEL     '1'           $F03
057000000000     C                   MOVEL     '1'           $ERR
057100000000     C                   MOVEL     '1'           $FINE
057200000000     C*
057300000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
057400000000X   1C                   ELSE
057500000000     C                   MOVEL     D60MSG        $MSG
057600000000     C                   EXSR      CARMSG
057700000000E   1C                   ENDIF
057800000000     C*
057900000000     C                   ENDSR
058000020923     C*-------------------------------------------------------------------------
058100020923     C* REPERISCE I DATI UTENTE
058200020923     C*-------------------------------------------------------------------------
058300020923     C     REPDATIUTE    BEGSR
058400020923     C*
058500020923     C* INIZIALIZZA VARIABILI DI WRK
058600020923     C                   CLEAR                   TIBS34DS
058700020923     C                   CLEAR                   AZUTEDS
058800020923     C                   CLEAR                   DDATIUTE
058900020923     C*
059000020923     C     *DTAARA       DEFINE    §azute        azuteds
059100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
059200020923     C                   IN(E)     *DTAARA
059300020923if  1C                   IF        %Error
059400020923     c                   EVAL      I34Tla = 'L'
059500020923     C                   CALL      'TIBS34R'
059600020923     C                   PARM                    Tibs34Ds
059700020923     C                   IN        *DTAARA
059800020923e   1C                   ENDIF
059900020923     C*
060000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
060100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
060200020923     C*
060300020923     C                   ENDSR
060400000000     C*------------------------------------------------------------------------*
060500000000     C*  *INZSR - ROUTINE INIZIALE
060600000000     C*------------------------------------------------------------------------*
060700000000     C     *INZSR        BEGSR
060800000000     C*
060900000000     C* RICEVIMENTO PARAMETRI
061000000000     C     *ENTRY        PLIST
061100000000     C                   PARM                    KPJBA
061200000000     C                   MOVEL     KPJBU         DS60
061300000000     C*
061400000000     C* DEFINIZIONE KLIST
061500000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
061600000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
061700000000     C     *LIKE         DEFINE    ECTDDC        KCTDDC
061800000000     C     *LIKE         DEFINE    ECTNBC        WNBC
061900000000     C     *LIKE         DEFINE    ECTCMP        WCMP
062000000000     C     *LIKE         DEFINE    ECTDDC        WDDC
062100000000     C*
062200000000     C* DEFINIZIONE DEPOSITI
062300000000     C     *LIKE         DEFINE    VCCNBC        DEPNBC
062400000000     C     *LIKE         DEFINE    VCCCMP        DEPCMP
062500000000     C     *LIKE         DEFINE    VCCDDC        DEPDDC
062600000000     C*
062700000000     C* CHIAVE LETTURA ECECT01L - COMPLETA
062800000000     C     KEYECT        KLIST
062900000000     C                   KFLD                    KCTNBC
063000000000     C                   KFLD                    KCTCMP
063100000000     C                   KFLD                    KCTDDC
063200000000     C*
063300000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
063400000000     C                   MOVE      D60OP0        INTGES            2
063500000000IF  1C     INTGES        IFEQ      '02'
063600000000     C     D60OP1        ANDNE     'O01'
063700000000     C                   MOVEL     'G'           $TGES             1
063800000000     C                   MOVEL     DESGES        VTDOPE
063900000000X   1C                   ELSE
064000000000     C                   MOVEL     'I'           $TGES             1
064100000000     C                   MOVEL     DESINT        VTDOPE
064200000000E   1C                   ENDIF
064300000000     C*
064400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
064500000000     C                   EXSR      INZSTD
064600000000     C*
064700000000     C* SE RICHIAMATO PER LA SCELTA CON IL SFL (OPZIONE 1)
064800000000     C* INIBISCO LE ALTRE OPZIONI
064900000000IF  1C     D60OP1        IFEQ      'O01'
065000000000     C                   MOVEL     OPZ(5)        FUNRIG
065100000000     C                   MOVEL     'N'           FUNASG
065200000000     C                   MOVEL     'N'           FUNASI
065300000000     C                   MOVEL     'N'           FUNAFG
065400000000     C                   MOVEL     'N'           FUNAFG
065500000000     C                   MOVEL     FUNRIG        OPZ(5)
065600000000     C                   MOVEL     OPZ(11)       FUNRIG
065700000000     C                   MOVEL     'N'           FUNASG
065800000000     C                   MOVEL     'N'           FUNASI
065900000000     C                   MOVEL     'N'           FUNAFG
066000000000     C                   MOVEL     'N'           FUNAFG
066100000000     C                   MOVEL     FUNRIG        OPZ(11)
066101040209     C                   MOVEL     OPZ(12)       FUNRIG
066102040209     C                   MOVEL     'N'           FUNASG
066103040209     C                   MOVEL     'N'           FUNASI
066104040209     C                   MOVEL     'N'           FUNAFG
066105040209     C                   MOVEL     'N'           FUNAFG
066106040209     C                   MOVEL     FUNRIG        OPZ(12)
066200000000X   1C                   ELSE
066300000000     C                   MOVEL     OPZ(1)        FUNRIG
066400000000     C                   MOVEL     'N'           FUNASG
066500000000     C                   MOVEL     'N'           FUNASI
066600000000     C                   MOVEL     'N'           FUNAFG
066700000000     C                   MOVEL     'N'           FUNAFG
066800000000     C                   MOVEL     FUNRIG        OPZ(1)
066900000000E   1C                   ENDIF
067000000000     C*
067100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
067200000000     C                   EXSR      INZSFL
067300000000     C*
067400000000     C* IMPOSTO CAMPI TESTATA VIDEO
067500000000     C                   MOVEL     DSFIRS        VTDRSU
067600000000     C*
067700000000     C* DEVO REPERIRE IL NUMERO BLOCCO, L'ULTIMO IN ARCHIVIO
067800940721     C     *HIVAL        SETGT     ECECT01L
067900000000     C                   READP     ECECT01L                               99
068000000000     C   99              Z-ADD     1             VCCNBC
068100000000     C  N99              Z-ADD     ECTNBC        VCCNBC
068200000000     C*
068300000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
068400000000     C                   Z-ADD     *ZEROS        VCCCMP
068500000000     C                   Z-ADD     *ZEROS        VCCDDC
068501040209     c* se ricevuti parametri li imposto
068502040209     c                   if        d60cmp <> 0
068503040209     c                   z-add     d60cmp        vcccmp
068508040209     c                   end
068600000000     C*
068700000000     C* EMETTO VIDEATA SUBFILE
068800000000     C                   EXSR      CARVS
068900000000     C*
069000000000     C                   ENDSR
069100000000     C*------------------------------------------------------------------------*
069200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
069500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
069600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
069700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069900000000     C*------------------------------------------------------------------------*
070000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
070100000000     C*------------------------------------------------------------------------*
070200000000     C     INZSTD        BEGSR
070300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
070400000000     C                   CALL      'XKEYS'
070500000000     C                   PARM                    DSKEYS
070600000000     C                   MOVEA     KEYRIG        KEYS
070700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
070800000000     C     XMSGPA        PLIST
070900000000     C                   PARM                    DSMSMS
071000000000     C                   PARM                    DSMSPG
071100000000     C                   PARM                    DSMSKE
071200000000     C                   PARM                    DSMSER
071300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
071400000000     C                   MOVEL     'TREC72R'     DSMSPG
071500000000     C                   MOVEL     'TREC72R'     VMCQUE
071600000000     C                   MOVEL     *BLANKS       VMCKEY
071700000000     C                   MOVEL     *BLANKS       DSMSMS
071800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
071900000000     C                   MOVEL     '0'           $ERR              1
072000000000     C                   MOVEL     '0'           $F03              1
072100000000     C                   MOVEL     *BLANKS       $GES              2
072200000000     C                   MOVEL     *BLANKS       $MSG             80
072300000000     C                   MOVEL     '0'           $FINE             1
072400000000     C                   MOVEL     '0'           $TESVD            1
072500000000     C* LANCIO ROUTINE PER VERIFICARE SE
072600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
072700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
072800020923     C                   EXSR      REPDATIUTE
072900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
073000000000     C                   BITOFF    '01234567'    POSMI1            1
073100000000     C                   BITOFF    '01234567'    POSMI2            1
073200000000     C                   BITOFF    '01234567'    POSMA1            1
073300000000     C                   BITOFF    '01234567'    POSMA2            1
073400000000     C                   BITON     '34'          POSMI1            1
073500000000     C                   BITON     '7'           POSMI2            1
073600000000     C                   BITON     '34'          POSMA1            1
073700000000     C                   BITON     '0'           POSMA2            1
073800000000     C                   MOVE      POSMI2        POSMIN            2
073900000000     C                   MOVEL     POSMI1        POSMIN
074000000000     C                   MOVE      POSMA2        POSMAX            2
074100000000     C                   MOVEL     POSMA1        POSMAX
074200000000     C                   ENDSR
074300000000     C*------------------------------------------------------------------------*
074400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
074500000000     C*------------------------------------------------------------------------*
074600000000     C     FUNSTD        BEGSR
074700000000     C                   SELECT
074800000000     C     KEY           WHENEQ    F3
074900000000     C                   EXSR      F03STD
075000000000     C     KEY           WHENEQ    F12
075100000000     C                   EXSR      F12STD
075200000000     C     KEY           WHENEQ    F24
075300000000     C                   EXSR      F24STD
075400000000     C                   OTHER
075500000000     C                   MOVEL     MSG(2)        $MSG
075600000000     C                   EXSR      CARMSG
075700000000     C                   ENDSL
075800000000     C                   ENDSR
075900000000     C*------------------------------------------------------------------------*
076000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
076100000000     C*------------------------------------------------------------------------*
076200000000     C     F03STD        BEGSR
076300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
076400000000     C                   MOVEL     '1'           $F03
076500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
076600000000     C                   MOVEL     '1'           $FINE
076700000000     C                   ENDSR
076800000000     C*------------------------------------------------------------------------*
076900000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
077000000000     C*------------------------------------------------------------------------*
077100000000     C     F12STD        BEGSR
077200000000     C* IMPOSTO FLAG DI FIN PGM
077300000000     C                   MOVEL     '1'           $FINE
077400000000     C                   ENDSR
077500000000     C*------------------------------------------------------------------------*
077600000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
077700000000     C*------------------------------------------------------------------------*
077800000000     C     F24STD        BEGSR
077900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
078000000000     C                   EXSR      OUTFUN
078100000000     C                   ENDSR
078200000000     C*------------------------------------------------------------------------*
078300000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
078400000000     C*------------------------------------------------------------------------*
078500000000     C     INZSFL        BEGSR
078600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
078700000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
078800000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
078900000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
079000000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
079100000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
079200000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
079300000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
079400000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
079500000000     C                   Z-ADD     12            SFLPAG
079600000000     C                   ENDSR
079700000000     C*------------------------------------------------------------------------*
079800000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
079900000000     C*------------------------------------------------------------------------*
080000000000     C     FUNSFL        BEGSR
080100000000     C                   SELECT
080200000000     C     KEY           WHENEQ    ENTER
080300000000     C                   EXSR      ENTSFL
080400000000     C     KEY           WHENEQ    F3
080500000000     C                   EXSR      F03STD
080600000000     C     KEY           WHENEQ    F5
080700000000     C                   EXSR      F05STD
080800000000     C     KEY           WHENEQ    F12
080900000000     C                   EXSR      F12STD
081000000000     C     KEY           WHENEQ    F17
081100000000     C                   EXSR      F17STD
081200000000     C     KEY           WHENEQ    F18
081300000000     C                   EXSR      F18STD
081400000000     C     KEY           WHENEQ    F23
081500000000     C                   EXSR      F23STD
081600000000     C     KEY           WHENEQ    F24
081700000000     C                   EXSR      F24STD
081800000000     C     KEY           WHENEQ    ROLLUP
081900000000     C                   EXSR      RUPSTD
082000000000     C     KEY           WHENEQ    ROLLDN
082100000000     C                   EXSR      RDNSTD
082200000000     C                   OTHER
082300000000     C                   MOVEL     MSG(2)        $MSG
082400000000     C                   EXSR      CARMSG
082500000000     C                   ENDSL
082600000000     C                   ENDSR
082700000000     C*------------------------------------------------------------------------*
082800000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
082900000000     C*------------------------------------------------------------------------*
083000000000     C     F05STD        BEGSR
083100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
083200000000     C                   EXSR      POSSFL
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
083600000000     C*------------------------------------------------------------------------*
083700000000     C     F17STD        BEGSR
083800000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
083900000000     C                   Z-ADD     1             VCCNBR
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
084300000000     C*------------------------------------------------------------------------*
084400000000     C     F18STD        BEGSR
084500000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
084600000000     C                   Z-ADD     SFLULT        VCCNBR
084700000000     C                   ENDSR
084800000000     C*------------------------------------------------------------------------*
084900000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
085000000000     C*------------------------------------------------------------------------*
085100000000     C     F23STD        BEGSR
085200000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
085300000000     C                   EXSR      OUTOPZ
085400000000     C                   ENDSR
085500000000     C*------------------------------------------------------------------------*
085600000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
085700000000     C*------------------------------------------------------------------------*
085800000000     C     RUPSTD        BEGSR
085900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
086000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
086100000000     C     CURSOR        IFGE      POSMIN
086200000000     C     CURSOR        ANDLE     POSMAX
086300000000     C                   ELSE
086400000000     C                   EXSR      CARSFL
086500000000     C                   ENDIF
086600000000     C                   ENDSR
086700000000     C*------------------------------------------------------------------------*
086800000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
086900000000     C*------------------------------------------------------------------------*
087000000000     C     RDNSTD        BEGSR
087100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
087200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
087300000000     C     CURSOR        IFGE      POSMIN
087400000000     C     CURSOR        ANDLE     POSMAX
087500000000     C                   ELSE
087600000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
087700000000     C                   MOVEL     '1'           $ERR
087800000000     C                   MOVEL     MSG(5)        $MSG
087900000000     C                   EXSR      CARMSG
088000000000     C                   ENDIF
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
088400000000     C*------------------------------------------------------------------------*
088500000000     C     ENTSFL        BEGSR
088600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
088700000000     C                   EXSR      NEWPOS
088800000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
088900000000     C     $ERR          IFEQ      '0'
089000000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
089100000000     C     SWPOSI        IFNE      0
089200000000     C                   EXSR      POSSFL
089300000000     C                   ELSE
089400000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
089500000000     C     SFLULT        IFNE      0
089600000000     C                   EXSR      OPZSFL
089700000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
089800000000     C     SWMODI        IFNE      0
089900000000     C                   EXSR      POSSFL
090000000000     C                   ENDIF
090100000000     C                   ENDIF
090200000000     C                   ENDIF
090300000000     C                   ENDIF
090400000000     C                   ENDSR
090500000000     C*------------------------------------------------------------------------*
090600000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
090700000000     C*------------------------------------------------------------------------*
090800000000     C     POSSFL        BEGSR
090900000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
091000000000     C                   EXSR      CLRSFL
091100000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
091200000000     C                   EXSR      SETFIL
091300000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
091400000000     C     SFLEOF        IFEQ      0
091500000000     C                   EXSR      CARSFL
091600000000     C                   ENDIF
091700000000     C                   ENDSR
091800000000     C*------------------------------------------------------------------------*
091900000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
092000000000     C*------------------------------------------------------------------------*
092100000000     C     OPZSFL        BEGSR
092200000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
092300000000     C                   MOVEL     'S'           OPZOK             1
092400000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
092500000000     C                   Z-ADD     0             SFLVAR            5 0
092600000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
092700000000     C                   Z-ADD     0             SFLERR
092800000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
092900000000     C                   Z-ADD     SFLULT        WFLULT
093000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
093100000000     C                   READC     EC72DVS                                49
093200000000     C     *IN49         DOWEQ     '0'
093300941121     C     $ERR          ANDEQ     '0'
093400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
093500000000     C                   ADD       1             SFLVAR
093600000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
093700000000     C     OPZOK         IFEQ      'S'
093800000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
093900000000     C     VSCOPZ        IFNE      *BLANKS
094000000000     C                   EXSR      CONOPZ
094100000000     C     OPZOK         IFEQ      'S'
094200941121     C     $ERR          ANDEQ     '0'
094300000000     C                   EXSR      GESOPZ
094400000000     C                   ENDIF
094500000000     C                   ENDIF
094600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
094700000000     C*            2. PULIZIA SCELTA IN SFL
094800000000     C     OPZOK         IFEQ      'S'
094900000000     C                   MOVEL     '0'           *IN41
095000000000     C                   MOVEL     *BLANKS       VSCOPZ
095100000000     C                   ELSE
095200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
095300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
095400000000     C                   MOVEL     '1'           *IN41
095500000000     C                   Z-ADD     SFLULT        SFLERR
095600000000     C                   ENDIF
095700000000     C                   ELSE
095800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
095900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
096000000000     C                   MOVEL     '1'           *IN41
096100000000     C                   ENDIF
096200000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
096300000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
096400000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
096500000000     C     OPZOK         IFNE      'S'
096600000000     C                   Z-ADD     SFLERR        VCCNBR
096700000000     C                   ELSE
096800000000     C                   Z-ADD     SFLULT        VCCNBR
096900000000     C                   ENDIF
097000000000     C* AGGIORNO RECORD SFL
097100000000     C                   UPDATE    EC72DVS
097200941121     C     $ERR          IFEQ      '0'
097300000000     C                   READC     EC72DVS                                49
097400941121     C                   ENDIF
097500000000     C                   ENDDO
097600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
097700000000     C                   Z-ADD     WFLULT        SFLULT
097800000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
097900000000     C     SFLVAR        IFEQ      0
098000000000     C                   MOVEL     '1'           $FINE
098100000000     C                   ENDIF
098200000000     C                   ENDSR
098300000000     C*------------------------------------------------------------------------*
098400000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
098500000000     C*------------------------------------------------------------------------*
098600000000     C     CLRSFL        BEGSR
098700000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
098800000000     C* (0=NON FINE FILE  1=FINE FILE)
098900000000     C                   Z-ADD     0             SFLEOF
099000000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
099100000000     C                   Z-ADD     0             SFLULT
099200000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
099300000000     C                   MOVEL     '1'           *IN40
099400000000     C                   WRITE     EC72DVC
099500000000     C                   MOVEL     '0'           *IN40
099600000000     C                   ENDSR
099700000000     C*------------------------------------------------------------------------*
099800000000     C* CARSFL - CARICAMENTO SFL
099900000000     C*------------------------------------------------------------------------*
100000000000     C     CARSFL        BEGSR
100100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
100200000000     C     SFLULT        IFEQ      0
100300000000     C                   EXSR      SETFIL
100400000000     C                   ENDIF
100500000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
100600000000     C                   Z-ADD     SFLULT        WFLULT
100700000000     C* CICLO:
100800000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100900000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
101000000000     C                   Z-ADD     1             SFLRIG
101100000000     C     SFLRIG        DOWLE     SFLPAG
101200000000     C     SFLEOF        ANDEQ     0
101300000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
101400000000     C                   MOVEL     *BLANKS       VSCOPZ
101500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
101600000000     C                   EXSR      IRESFL
101700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
101800000000     C                   ADD       1             SFLULT
101900000000     C* SCRITTURA RECORD SFL
102000000000     C                   WRITE     EC72DVS
102100000000     C                   ADD       1             SFLRIG
102200000000     C* LETTURA FILE DA CARICARE
102300000000     C                   EXSR      LETFIL
102400000000     C                   ENDDO
102500000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
102600000000     C     WFLULT        IFNE      SFLULT
102700000000     C                   Z-ADD     WFLULT        VCCNBR
102800000000     C                   ADD       1             VCCNBR
102900000000     C                   ENDIF
103000000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
103100000000     C     SFLEOF        IFEQ      1
103200000000     C     SFLULT        ANDGT     *ZEROS
103300000000     C                   MOVEL     '1'           $ERR
103400000000     C                   MOVEL     MSG(6)        $MSG
103500000000     C                   EXSR      CARMSG
103600000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
103700000000     C     SFLULT        CHAIN     EC72DVS                            99
103800000000     C     *IN99         IFEQ      '0'
103900000000     C                   SETON                                        45
104000000000     C                   UPDATE    EC72DVS
104100000000     C                   SETOFF                                       45
104200000000     C                   ENDIF
104300000000     C                   ENDIF
104400000000     C                   ENDSR
104500000000     C*------------------------------------------------------------------------*
104600000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
104700000000     C*------------------------------------------------------------------------*
104800000000     C     OUTSFL        BEGSR
104900000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
105000000000     C     MSGULT        IFNE      *ZEROS
105100000000     C                   EXSR      OUTMSG
105200000000     C                   ENDIF
105300000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
105400000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
105500000000     C     SFLEOF        IFEQ      0
105600000000     C                   MOVEL     '0'           *IN42
105700000000     C                   ELSE
105800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
105900000000     C                   MOVEL     '1'           *IN42
106000000000     C                   ENDIF
106100000000     C* EMISSIONE SUBFILE
106200000000     C     SFLULT        IFLE      0
106300000000     C*  --> CASO DI SFL VUOTO
106400000000     C                   MOVEL     '0'           *IN43
106500000000     C                   WRITE     EC72DVV
106600000000     C                   WRITE     EC72DVC
106700000000     C                   READ      EC72DVC                                99
106800000000     C                   ELSE
106900000000     C*  --> CASO DI SFL CON RECORDS
107000000000     C                   WRITE     EC72DVC
107100000000     C                   READ      EC72DVC                                99
107200000000     C                   ENDIF
107300000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
107400000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
107500000000     C                   Z-ADD     FRTSFL        VCCNBR
107600000000     C                   ENDSR
107700000000     C*------------------------------------------------------------------------*
107800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
107900000000     C*          CODA MESSAGGI DEL PGM
108000000000     C*------------------------------------------------------------------------*
108100000000     C     CARMSG        BEGSR
108200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
108300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
108400000000     C     MSGULT        IFEQ      0
108500000000     C                   MOVEL     '1'           *IN40
108600000000     C                   WRITE     EC72DVK
108700000000     C                   MOVEL     '0'           *IN40
108800000000     C                   ENDIF
108900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
109000000000     C                   MOVEL     $MSG          DSMSMS
109100000000     C                   CALL      'XMSG'        XMSGPA
109200000000     C     DSMSER        IFEQ      0
109300000000     C                   MOVEL     DSMSKE        VMCKEY
109400000000     C                   ENDIF
109500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
109600000000     C                   ADD       1             MSGULT            4 0
109700000000     C                   WRITE     EC72DVM
109800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
109900000000     C                   MOVEL     *BLANKS       $MSG
110000000000     C                   ENDSR
110100000000     C*------------------------------------------------------------------------*
110200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
110300000000     C*------------------------------------------------------------------------*
110400000000     C     OUTMSG        BEGSR
110500000000     C* VISUALIZZAZIONE MESSAGGIO
110600000000     C                   MOVEL     '1'           *IN44
110700000000     C                   WRITE     EC72DVK
110800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
110900000000     C                   Z-ADD     *ZEROS        MSGULT
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
111300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
111400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
111500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
111600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
111700000000     C*------------------------------------------------------------------------*
111800000000     C     CARFUN        BEGSR
111900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
112000000000     C     1             DO        31            I                 5 0
112100000000     C                   MOVEL     FUN(I)        FUNRIG
112200000000     C                   MOVEL     FUNFUN        FFU(I)
112300000000     C                   MOVEL     FUNDES        FDE(I)
112400020923     C     DUTLPO        IFEQ      'S'
112500000000     C     $TGES         IFEQ      'G'
112600000000     C                   MOVEL     FUNASG        FAT(I)
112700000000     C                   ELSE
112800000000     C                   MOVEL     FUNASI        FAT(I)
112900000000     C                   ENDIF
113000000000     C                   ELSE
113100000000     C     $TGES         IFEQ      'G'
113200000000     C                   MOVEL     FUNAFG        FAT(I)
113300000000     C                   ELSE
113400000000     C                   MOVEL     FUNAFI        FAT(I)
113500000000     C                   ENDIF
113600000000     C                   ENDIF
113700000000     C     FAT(I)        IFEQ      'E'
113800000000     C                   MOVEL     USER          WEDP              3
113900000000     C     WEDP          IFEQ      'EDP'
114000000000     C                   MOVEL     'S'           FAT(I)
114100000000     C                   ELSE
114200000000     C                   MOVEL     'N'           FAT(I)
114300000000     C                   ENDIF
114400000000     C                   ENDIF
114500000000     C                   ENDDO
114600000000     C                   ENDSR
114700000000     C*------------------------------------------------------------------------*
114800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
114900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
115000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
115100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
115200000000     C*------------------------------------------------------------------------*
115300000000     C     OUTFUN        BEGSR
115400000000     C     1             DO        8             I
115500000000     C                   MOVEL     *BLANKS       FUV(I)
115600000000     C                   ENDDO
115700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
115800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
115900000000     C     1             DO        31            I
116000000000     C     FAT(I)        IFEQ      'S'
116100000000     C     FDE(I)        ANDNE     *BLANKS
116200000000     C                   ADD       1             FUNNAT
116300000000     C                   ENDIF
116400000000     C                   ENDDO
116500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
116600000000     C     FUNNAT        IFGT      8
116700000000     C                   MOVEL     'S'           FAT(24)
116800000000     C                   ENDIF
116900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
117000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
117100000000     C                   Z-ADD     8             FUNVIS            5 0
117200000000     C     FAT(23)       IFEQ      'S'
117300000000     C                   SUB       1             FUNVIS
117400000000     C                   ENDIF
117500000000     C     FAT(24)       IFEQ      'S'
117600000000     C                   SUB       1             FUNVIS
117700000000     C                   ENDIF
117800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
117900000000     C                   Z-ADD     *ZEROS        W                 5 0
118000000000     C                   ADD       1             FUNULT
118100000000     C     FUNULT        IFGT      31
118200000000     C                   Z-ADD     1             FUNULT
118300000000     C                   ENDIF
118400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
118500000000     C     FUNULT        DO        31            I
118600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
118700000000     C     W             IFLT      FUNVIS
118800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
118900000000     C     FAT(I)        IFEQ      'S'
119000000000     C     FDE(I)        ANDNE     *BLANKS
119100000000     C     I             IFEQ      23
119200000000     C     I             OREQ      24
119300000000     C                   ELSE
119400000000     C                   ADD       1             W
119500000000     C                   MOVEL     FDE(I)        FUV(W)
119600000000     C                   ENDIF
119700000000     C                   ENDIF
119800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
119900000000     C                   Z-ADD     I             FUNULT
120000000000     C                   ENDIF
120100000000     C                   ENDDO
120200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
120300000000     C                   MOVEL     FUV(1)        VFD001
120400000000     C                   MOVEL     FUV(2)        VFD002
120500000000     C                   MOVEL     FUV(3)        VFD003
120600000000     C                   MOVEL     FUV(4)        VFD004
120700000000     C                   MOVEL     FUV(5)        VFD005
120800000000     C                   MOVEL     FUV(6)        VFD006
120900000000     C     FAT(23)       IFEQ      'N'
121000000000     C     FAT(24)       ANDEQ     'N'
121100000000     C                   MOVEL     FUV(7)        VFD007
121200000000     C                   MOVEL     FUV(8)        VFD008
121300000000     C                   ELSE
121400000000     C     FAT(23)       IFEQ      'S'
121500000000     C     FAT(24)       ANDEQ     'S'
121600000000     C                   MOVEL     FDE(23)       VFD007
121700000000     C                   MOVEL     FDE(24)       VFD008
121800000000     C                   ELSE
121900000000     C                   MOVEL     FUV(7)        VFD007
122000000000     C     FAT(23)       IFEQ      'S'
122100000000     C                   MOVEL     FDE(23)       VFD008
122200000000     C                   ELSE
122300000000     C                   MOVEL     FDE(24)       VFD008
122400000000     C                   ENDIF
122500000000     C                   ENDIF
122600000000     C                   ENDIF
122700000000     C                   MOVEL     '1'           $TESVD
122800000000     C                   ENDSR
122900000000     C*------------------------------------------------------------------------*
123000000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
123100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
123200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
123300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
123400000000     C*------------------------------------------------------------------------*
123500000000     C     CONFUN        BEGSR
123600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
123700000000     C     1             DO        31            I
123800000000     C     KEY           IFEQ      KEYS(I)
123900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
124000000000     C     FAT(I)        IFNE      'S'
124100000000     C                   MOVEL     MSG(1)        $MSG
124200000000     C                   EXSR      CARMSG
124300000000     C                   MOVEL     '1'           $ERR
124400000000     C                   ENDIF
124500000000     C                   ENDIF
124600000000     C                   ENDDO
124700000000     C                   ENDSR
124800000000     C*------------------------------------------------------------------------*
124900000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
125000000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
125100000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
125200000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
125300000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
125400000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
125500000000     C*------------------------------------------------------------------------*
125600000000     C     CAROPZ        BEGSR
125700000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
125800000000     C     1             DO        24            I
125900000000     C                   MOVEL     OPZ(I)        OPZRIG
126000000000     C                   MOVEL     OPZOPD        OPD(I)
126100000000     C                   MOVEL     OPZOPS        OPS(I)
126200000000     C                   MOVEL     OPZDES        ODE(I)
126300020923     C     DUTLPO        IFEQ      'S'
126400000000     C     $TGES         IFEQ      'G'
126500000000     C                   MOVEL     OPZASG        OAT(I)
126600000000     C                   ELSE
126700000000     C                   MOVEL     OPZASI        OAT(I)
126800000000     C                   ENDIF
126900000000     C                   ELSE
127000000000     C     $TGES         IFEQ      'G'
127100000000     C                   MOVEL     OPZAFG        OAT(I)
127200000000     C                   ELSE
127300000000     C                   MOVEL     OPZAFI        OAT(I)
127400000000     C                   ENDIF
127500000000     C                   ENDIF
127600000000     C     OAT(I)        IFEQ      'E'
127700000000     C                   MOVEL     USER          WEDP              3
127800000000     C     WEDP          IFEQ      'EDP'
127900000000     C                   MOVEL     'S'           OAT(I)
128000000000     C                   ELSE
128100000000     C                   MOVEL     'N'           OAT(I)
128200000000     C                   ENDIF
128300000000     C                   ENDIF
128400000000     C                   ENDDO
128500000000     C                   ENDSR
128600000000     C*------------------------------------------------------------------------*
128700000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
128800000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
128900000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
129000000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
129100000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
129200000000     C*------------------------------------------------------------------------*
129300000000     C     OUTOPZ        BEGSR
129400000000     C     1             DO        8             I
129500000000     C                   MOVEL     *BLANKS       OPV(I)
129600000000     C                   ENDDO
129700000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
129800000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
129900000000     C                   ADD       1             OPZULT
130000000000     C     OPZULT        IFGT      24
130100000000     C                   Z-ADD     1             OPZULT
130200000000     C                   END
130300000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
130400000000     C     1             DO        24            I
130500000000     C     OAT(I)        IFEQ      'S'
130600000000     C     ODE(I)        ANDNE     *BLANKS
130700000000     C                   ADD       1             OPZNAT
130800000000     C                   ENDIF
130900000000     C                   ENDDO
131000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
131100000000     C                   Z-ADD     0             W
131200000000     C     OPZULT        DO        24            I
131300000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
131400000000     C     W             IFLT      8
131500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
131600000000     C     OAT(I)        IFEQ      'S'
131700000000     C     ODE(I)        ANDNE     *BLANKS
131800000000     C                   ADD       1             W
131900000000     C                   MOVEL     ODE(I)        OPV(W)
132000000000     C                   ENDIF
132100000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
132200000000     C                   Z-ADD     I             OPZULT
132300000000     C                   ENDIF
132400000000     C                   ENDDO
132500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
132600000000     C                   MOVEL     OPV(1)        VOD001
132700000000     C                   MOVEL     OPV(2)        VOD002
132800000000     C                   MOVEL     OPV(3)        VOD003
132900000000     C                   MOVEL     OPV(4)        VOD004
133000000000     C                   MOVEL     OPV(5)        VOD005
133100000000     C                   MOVEL     OPV(6)        VOD006
133200000000     C                   MOVEL     OPV(7)        VOD007
133300000000     C                   MOVEL     OPV(8)        VOD008
133400000000     C                   MOVEL     '1'           $TESVD
133500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
133600000000     C     OPZNAT        IFGT      8
133700000000     C                   MOVEL     'S'           FAT(23)
133800000000     C                   ENDIF
133900000000     C                   ENDSR
134000000000     C*------------------------------------------------------------------------*
134100000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
134200000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
134300000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
134400000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
134500000000     C*------------------------------------------------------------------------*
134600000000     C     CONOPZ        BEGSR
134700000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
134800000000     C                   MOVEL     *BLANKS       OPZOK             1
134900000000     C     1             DO        24            I                 5 0
135000000000     C     VSCOPZ        IFEQ      OPD(I)
135100000000     C     VSCOPZ        OREQ      OPS(I)
135200000000     C     OAT(I)        IFEQ      'S'
135300000000     C                   MOVEL     OPD(I)        VSCOPZ
135400000000     C                   MOVEL     'S'           OPZOK
135500000000     C                   ELSE
135600000000     C                   MOVEL     'N'           OPZOK             1
135700000000     C                   ENDIF
135800000000     C                   ENDIF
135900000000     C                   ENDDO
136000000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
136100000000     C     OPZOK         IFNE      'S'
136200000000     C                   MOVEL     MSG(3)        $MSG
136300000000     C                   EXSR      CARMSG
136400000000     C                   MOVEL     '1'           $ERR
136500000000     C                   MOVEL     'N'           OPZOK             1
136600000000     C                   ENDIF
136700000000     C                   ENDSR
136800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
136900000000TREC72R- Tasto funzionale non valido.                                          1
137000000000TREC72R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
137100000000TREC72R- Opzione non valida.                                                   3
137200000000TREC72R- Opzione abilitata ma non gestita. Avvertire CED.                      4
137300000000TREC72R- Inizio elenco.                                                        5
137400000000TREC72R- Fine elenco.                                                          6
137500000000TREC72R- 7
137600000000TREC72R- 8
137700000000TREC72R- Immettere data formalmente valida                                     9
137800000000TREC72R- 10
137900000000TREC72R- 11
138000000000TREC72R- Codice competenza annullato, consentito solo il ripristino           12
138100000000TREC72R- Codice competenza non annullato, impossibile ripristinare            13
138200000000TREC72R- 14
138300000000TREC72R- 15
138400000000TREC72R- 16
138500000000TREC72R- 17
138600000000TREC72R- 18
138700000000TREC72R- 19
138800000000TREC72R- 20
138900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
139000000000- 1-1 -1=Scelta           -    SS     SS
139100000000- 2-2 -2=Modifica         -    SN     NN
139200000000- 3-3 -                   -    NN     NN
139300000000- 4-4 -4=Cancellazione    -    SN     NN
139400000000- 5-5 -5=Visualizzazione  -    SS     SS
139500000000- 6-6 -                   -    NN     NN
139600000000- 7-7 -7=Ripristino       -    SN     NN
139700000000- 8-8 -                   -    NN     NN
139800000000- 9-9 -                   -    NN     NN
139900000000-10-  -                   -    NN     NN
140000000000-11-11-11=Conti contab.tà -    SS     SS
140100040209-12-12-12=Voci            -    SS     SS
140200000000-13-  -                   -    NN     NN
140300000000-14-  -                   -    NN     NN
140400000000-15-  -                   -    NN     NN
140500000000-16-  -                   -    NN     NN
140600000000-17-  -                   -    NN     NN
140700000000-18-  -                   -    NN     NN
140800000000-19-  -                   -    NN     NN
140900000000-20-  -                   -    NN     NN
141000000000-21-  -                   -    NN     NN
141100000000-22-  -                   -    NN     NN
141200000000-23-  -                   -    NN     NN
141300000000-24-  -                   -    NN     NN
141400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
141500000000F1    -                   -    NN     NN
141600000000F2    -                   -    NN     NN
141700000000F3    -F3=Fine            -    SS     SS
141800000000F4    -                   -    NN     NN
141900000000F5    -F5=Rivisualizza    -    SS     SS
142000000000F6    -F6=Inserimento     -    SN     NN
142100000000F7    -                   -    NN     NN
142200000000F8    -                   -    NN     NN
142300000000F9    -                   -    NN     NN
142400000000F10   -                   -    NN     NN
142500000000F11   -                   -    NN     NN
142600000000F12   -F12=Ritorno        -    SS     SS
142700000000F13   -                   -    NN     NN
142800000000F14   -                   -    NN     NN
142900000000F15   -                   -    NN     NN
143000000000F16   -                   -    NN     NN
143100961003F17   -F17=Inizio dati    -    NN     NN
143200961003F18   -F18=Fine dati      -    NN     NN
143300000000F19   -                   -    NN     NN
143400000000F20   -                   -    NN     NN
143500000000F21   -                   -    NN     NN
143600000000F22   -                   -    NN     NN
143700000000F23   -F23=Altre opzioni  -    NN     NN
143800000000F24   -F24=Altri tasti    -    NN     NN
143900000000ENTER -                   -    SS     SS
144000000000ROLLUP-                   -    SS     SS
144100000000ROLLDN-                   -    SS     SS
144200000000HELP  -                   -    NN     NN
144300000000PRINT -                   -    NN     NN
144400000000BSPACE-                   -    NN     NN
144500000000CLEAR -                   -    NN     NN

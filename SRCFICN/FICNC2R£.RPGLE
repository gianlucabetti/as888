000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114      * FICNC2R ******************************************************
000300000704      *                                                              *
000400000704      *         SVALORIZZAZIONE CONTEGGI PADRONCINI      - FILTRO -  *
000500000704      *         -----------------------------------      ----------  *
000600000704      *                                                              *
000700000704      ****************************************************************
000800011114     FFICNC2D   CF   E             WORKSTN
000900021202     Ffiapd01L  IF   E           K DISK
001000011114     Ffiftt02L  IF   E           K DISK
001100011114     Ffifst02L  IF   E           K DISK
001200011114     F                                     RENAME(fiftt000:fifst2)
001300000705     D L1              S              3  0 DIM(30)                              FIL GESTITE 1°
001400000705     D L6              S              3  0 DIM(30)                              FIL GESTITE 2°
002100000704     D* PASSAGGIO DATI A PROGRAMMA DI ELABORAZIONE SVALORIZZAZIONE
002200000704     D PARAM           DS
002300000704     D  VIDSIM                 1      1
002400000704     D  VIDLID                 2      4  0
002500000704     D  VIDPAD                 5      8
002600000704     D  PARCPD                 2      8  0
002700000704     D  VIDLIA                 9     11  0
002800000704     D  VIDPAA                12     15
002900000704     D  PARCPA                 9     15  0
003000000705     D  PARDTR                16     23  0
003100001124     D  PARDTA                24     31  0
003200000704     D CNCR80        E DS
003201021202     D fnlv24ds      E DS
003300000704     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003400000704     D KPJBA         E DS
003500000704     D* DS PER TRUL06R - CARICAMENTO £1
003600000704     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003700000704     D  LIN                    1     90  0
003800000705     D                                     DIM(30)                              FIL COMODO
003900000705     D WLBDAT          DS
004000000705     D  G02DAT                 1      8  0
004100000705     D  G02INV                 9     16  0
004200000705     D  G02ERR                17     17
004300000705     D  G02TGI                18     22  0
004400000705      ****************************************************************
004500000705      * RIEPILOGO INDICATORI:                                        *
004600000705      ****************************************************************
004700000704      * 03 - FINE                                                    *
004800000704      * 06 - CONFERMA ELABORAZIONE                                   *
004900001124      * 20 - ERRORE: DATA AL NON VALIDA                              *
005000000704      * 21 - ERRORE: NON E' POSSIBILE SVALORIZZARE I CONTEGGI        *
005100000704      * 22 - ERRORE: LINEA "DA" ERRATA                               *
005200000704      * 23 - ERRORE: LINEA "A" ERRATA                                *
005300000704      * 24 - ERRORE: PADRONCINO INESISTENTE (SE SECCO)               *
005400000704      * 25 - ERRORE: PADRONCINO "DA" MAGGIORE DI PADRONCINO "A"      *
005500001124      * 26 - ERRORE: DATA DAL NON VALIDA                             *
005600000704      * 27 - ERRORE: PADRONCINO "DAL" CON CODICE NON NUMERICO        *
005700000704      * 28 - ERRORE: PADRONCINO "AL" CON CODICE NON NUMERICO         *
005800000705      * 29 - ERRORE: NEL RANGE DI CODICI NESSUN PADRONCINO VALIDO    *
005900000704      * 30 - COMODO                                                  *
006000000704      * 35 - COMODO                                                  *
006100000719      * 38 - ON=SIMULAZIONE                                          *
006200000704      * 88 - HO RICHIESTO "?" LA RICERCA DI UN PADRONCINO            *
006300000704      * 89 - SONO UTENTE DI SECONDO LIVELLO                          *
006400000704      * 90 - ERRORE GENERICO A VIDEO                                 *
006500000705      ****************************************************************
006600000704      *
006700000705      ****************************************************************
006800000704      *                                                              *
006900000704      *         O P E R A Z I O N I   I N I Z I A L I                *
007000000704      *                                                              *
007100000704      ****************************************************************
007200000704      *
007300000704     C     *ENTRY        PLIST
007400000704     C                   PARM                    KPJBA
007500000705      *
007600000705     C     *LIKE         DEFINE    PARCPA        PARCPW
007700000705     C     *LIKE         DEFINE    FTTPDR        KPDR
007800000705     C     *LIKE         DEFINE    FTTFVL        KFVL
007900000705     C     *LIKE         DEFINE    FTTDDC        KDDC
008000000704      *
008100000705     C     KFTT          KLIST
008200000705     C                   KFLD                    KPDR
008300000705     C                   KFLD                    KFVL
008400000705     C                   KFLD                    KDDC
008401021202     C     Kapd          KLIST
008402021202     C                   KFLD                    apdtip
008403021202     C                   KFLD                    parcpd
008404021202     c                   movel     'A'           apdtip
008500000705      *
008600000704      * RECUPERO RAGIONE SOCIALE UTENTE E SIMFEL
008700000704      *
008800000704     C                   Z-ADD     1             CODUT
008900000704     C                   CALL      'X§PARUT'
009000000704     C                   PARM                    UT§DSE
009100000704     C                   MOVEL     RAGUT         VIDRSU
009200000704     C                   MOVEL     REC80         CNCR80
009300000704      *
009400000704      * CARICO L1 O L6 IN BASE ALL'UTENTE (SIMFEL O 2° LIVELLO)
009500000704      *
009600000704     C                   EXSR      CARTAB
009700000704      *                    -----------
009800000704      *
009900000704      * INIZIALIZZO ALTRI CAMPI VIDEO
010000000704      *
010100000704     C                   MOVE      ' '           VIDSIM
010200020606     c                   movel     knmus         trebyte           3
010201060119      * ?------------------------------------------------------------------ */
010302060119      * ?--  Non deve essere più eseguita la svalorizzazione in buona
010303060119      * ?--   da quando i Padroncini vanno in UNI
010304060119      * ?--  La svalorizzazione può avvenire solo in simulazione.
010305060119      * ?------------------------------------------------------------------ */
010306060119     c******             if        trebyte <> 'EDP'
010400020606     c                   move      'S'           vidsim
010500020606     c                   seton                                        66
010600060119     c*******            end
010700000704     C                   MOVE      *BLANKS       VIDPAD
010800000704     C                   MOVE      *BLANKS       VIDPAA
010900000704     C                   MOVE      *BLANKS       VIDDEP
011000000704     C                   Z-ADD     0             VIDDTR
011100000704     C                   Z-ADD     0             VIDDT2
011200000704     C                   MOVE      *BLANKS       VIDPA2
011300000704      *
011400000705      ****************************************************************
011500000704      *                                                              *
011600000704      *         R O U T I N E   P R I N C I P A L E                  *
011700000704      *                                                              *
011800000704      ****************************************************************
011900000704      *
012000000704     C     EMETTI        TAG
012100011114     C                   EXFMT     FICNC21
012200000704      *
012300000704      * F3=FINE
012400000704      *
012500000704     C   03              GOTO      FINE
012600000704      *
012700000704      * CONTROLLO PARAMETRI IMMESSI A VIDEO
012800000704      *
012900000704     C                   EXSR      CONTR
013000000704      *                    ----------
013100000704      *
013200000704      * RIEMETTO SE: 1) ERRORE 2) NO F6 3) E' STATO RICERCATO UN
013300000704      *              PADRONCINO CON "?"
013400000704      *
013500000704     C     *IN06         IFEQ      *OFF
013600000704     C     *IN88         OREQ      *ON
013700000704     C     *IN90         OREQ      *ON
013800000704     C                   GOTO      EMETTI
013900000704     C                   ENDIF
014000000704      *
014100000704      * LANCIO L'ELABORAZIONE
014200000705      * =====================
014300000705      *
014400000705     C                   MOVEL     PARAM         KPJBU
014500000705      *                    ==============
014600011114     C                   CALL      'FICNC3R'
014700000705      *                    ==============
014800000705     C                   PARM                    KPJBA
014900000704      *
015000000704      * FINE PROGRAMMA
015100000704      *
015200000704     C     FINE          TAG
015300000704     C                   SETON                                        LR
015400000704      *
015500000705      ****************************************************************
015600000704      *                                                              *
015700000704      *         S U B R O U T I N E S                                *
015800000704      *                                                              *
015900000704      ****************************************************************
016000000704      *
016100000704      *--------------------------------------------------------------*
016200000704      * CARTAB - SUBROUTINE CARICAMENTO TABELLE                      *
016300000704      *--------------------------------------------------------------*
016400000704      *
016500000704     C     CARTAB        BEGSR
016600000704      *
016700000704      * VEDO SE SONO SIMFEL O SECONDO LIVELLO
016800000704      *
016900020717     C                   MOVEL     simpou        VIDLID
017000020717     C                   MOVEL     simpou        VIDLIA
017100000704      * SONO 2° LIV.
017200020717     c                   if        simtpp = '2'
017300000704     C                   SETON                                        89
017400000704      *
017500000704      * CARICO TABELLA FILIALI GESTITE £6
017600000704      *
017700000704     C                   CLEAR                   DSUL06
017800000704     C                   MOVE      '£6'          D06COD
017900020717     C                   MOVEL     simpou        D06KEY
018000000704     C                   MOVEL     DSUL06        KPJBU
018100000704      *
018200000704     C                   CALL      'TRUL06R'
018300000704     C                   PARM                    KPJBA
018400000704     C                   MOVEL     KPJBU         DSUL06
018500000704     C                   MOVEA     LIN           L6
018600000704      *
018700000704     C                   ELSE
018800000704      * SONO SIMFEL
018900000704      *
019000000704      * CARICO TABELLA FILIALI GESTITE £1
019100000704      *
019200000704     C                   CLEAR                   DSUL06
019300000704     C                   MOVE      '£1'          D06COD
019400000704     C                   MOVEL     SIMFEL        D06KEY
019500000704     C                   MOVEL     DSUL06        KPJBU
019600000704     C*
019700000704     C                   CALL      'TRUL06R'
019800000704     C                   PARM                    KPJBA
019900000704     C                   MOVEL     KPJBU         DSUL06
020000000704     C                   MOVEA     LIN           L1
020100000704      *
020200000704     C                   ENDIF
020300000704      *
020400000704     C                   ENDSR
020500000704      *
020600000704      *--------------------------------------------------------------*
020700000704      * CONTR  - SUBROUTINE CONTROLLO DATI VIDEO                     *
020800000704      *--------------------------------------------------------------*
020900000704      *
021000000704     C     CONTR         BEGSR
021100000704      *
021200000704     C                   SETOFF                                       908821
021300000705     C                   SETOFF                                       38
021400000704      *
021500000705      * S I M U L A Z I O N E
021600000705      *
021700000705     C     VIDSIM        IFEQ      'S'
021800000705     C                   SETON                                        38
021900000705     C                   ENDIF
022000000705      *
022100000704      * F I L I A L E    P A D R O N C I N O    D A L
022200000704      *
022300000704      * FILIALE OBBLIGATORIA
022400000704      *
022500000704     C     VIDLID        IFEQ      0
022600000704     C                   SETON                                        2290
022700000704     C                   GOTO      ENDCT1
022800000704     C                   ENDIF
022900021015      * Forza la 2°LNP
023000021015     c                   move      vidlid        vidlia
023100000704      *
023200000704      * 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
023300000704      *
023400000704     C     *IN89         IFEQ      *OFF
023500000704     C     VIDLID        IFNE      SIMFEL
023600000704     C     VIDLID        LOOKUP    L1                                     35
023700000704     C  N35              SETON                                        2290
023800000704     C  N35              GOTO      ENDCT1
023900000704     C                   ENDIF
024000000704      *
024100000704     C                   ELSE
024200000704      *
024300000704      * 89 ON  - SONO UN 2° LIV. QUINDI CONTROLLO LA FILIALE NELLA L6
024400000704      *
024500020717     C     vidlid        IFNE      simpou
024600000704     C     VIDLID        LOOKUP    L6                                     35
024700000704     C  N35              SETON                                        2290
024800000704     C  N35              GOTO      ENDCT1
024900000704     C                   ENDIF
025000000704     C                   ENDIF
025100000704      *
025200000704      * F I L I A L E    P A D R O N C I N O    A L
025300000704      *
025400000704      * FILIALE OBBLIGATORIA
025500000704      *
025600000704     C     VIDLIA        IFEQ      0
025700000704     C                   SETON                                        2390
025800000704     C                   GOTO      ENDCT1
025900000704     C                   ENDIF
026000000704      *
026100000704      * 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
026200000704      *
026300000704     C     *IN89         IFEQ      *OFF
026400000704     C     VIDLIA        IFNE      SIMFEL
026500000704     C     VIDLIA        LOOKUP    L1                                     35
026600000704     C  N35              SETON                                        2390
026700000704     C  N35              GOTO      ENDCT1
026800000704     C                   ENDIF
026900000704      *
027000000704     C                   ELSE
027100000704      *
027200000704      * 89 ON  - SONO UN 2° LIV. QUINDI CONTROLLO LA FILIALE NELLA L6
027300000704      *
027400020717     C     vidlia        IFNE      simpou
027500000704     C     VIDLIA        LOOKUP    L6                                     35
027600020717     C  N35              SETON                                        2390
027700000704     C  N35              GOTO      ENDCT1
027800000704     C                   ENDIF
027900000704     C                   ENDIF
028000000704      *
028100000704      * P A D R O N C I N O
028200000704      *
028300000704      * RICERCA
028400000704      *
028500000704     C     '?'           SCAN      VIDPAD                                 88
028600000704      *
028700000704     C     *IN88         IFEQ      *ON
028800021202     C                   MOVEL     *BLANKS       d24rsc
028900021202     C                   Z-ADD     0             d24pdr
029000021202     C                   Z-ADD     VIDLID        d24FIL
029100021202     C                   MOVEL     'R'           d24FLG
029101021202     C                   MOVEL     'A'           d24tip
029200021202     C                   MOVEL(p)  fnlv24ds      KPJBU
029300000704     C                   CALL      'FNLV24R'
029400000704     C                   PARM                    KPJBA
029500021202     C                   MOVEL     KPJBU         fnlv24ds
029600000704      *
029700000704      * CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
029800000704      *
029900021202     C     d24pdr        IFNE      0
030000021202     C                   MOVEL     d24pdr        VIDLID
030100000704     C                   MOVEL     *ZEROS        VIDPAD
030200021202     C                   MOVE      d24pdr        VIDPAD
030300021202     C                   MOVEL     d24rsc        VIDDEP
030400000704     C                   ELSE
030500000704     C                   MOVE      *BLANKS       VIDDEP
030600000704     C                   MOVE      *BLANKS       VIDPAD
030700000704     C                   ENDIF
030800000704      *
030900000704     C                   GOTO      ENDCT1
031000000704     C                   ENDIF
031100000704      *
031200000704     C     '?'           SCAN      VIDPAA                                 88
031300000704      *
031400000704     C     *IN88         IFEQ      *ON
031700021202     C                   Z-ADD     VIDLIA        d24FIL
031701021202     C                   MOVEL     *BLANKS       d24rsc
031702021202     C                   Z-ADD     0             d24pdr
031704021202     C                   MOVEL     'R'           d24FLG
031705021202     C                   MOVEL     'A'           d24tip
031706021202     C                   MOVEL(p)  fnlv24ds      KPJBU
032000000704     C                   CALL      'FNLV24R'
032100000704     C                   PARM                    KPJBA
032200021202     C                   MOVEL     KPJBU         fnlv24ds
032300000704      *
032400000704      * CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
032500000704      *
032600021202     C     d24pdr        IFNE      0
032700021202     C                   MOVEL     d24pdr        VIDLIA
032800000704     C                   MOVEL     *ZEROS        VIDPAA
032900021202     C                   MOVE      d24pdr        VIDPAA
033000000704     C                   ELSE
033100000704     C                   MOVE      *BLANKS       VIDPAA
033200000704     C                   ENDIF
033300000704      *
033400000704     C                   GOTO      ENDCT1
033500000704     C                   ENDIF
033600000704      *
033700000704      * PADRONCINO DAL
033800000704      *
033900000704     C     VIDPAD        IFLE      *ZEROS
034000000704     C                   SETON                                        2790
034100000704     C                   GOTO      ENDCT1
034200000704     C                   ENDIF
034300000704      *
034400000704     C                   TESTN                   VIDPAD               35
034500000704     C     *IN35         IFEQ      *ON
034600000704     C                   MOVE      VIDPAD        W001              1
034700000704      * CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
034800000704     C     W001          IFLT      '0'
034900000704     C                   SETOFF                                       35
035000000704     C                   ENDIF
035100000704     C                   ENDIF
035200000704      *
035300000704     C  N35              SETON                                        2790
035400000704     C  N35              GOTO      ENDCT1
035500000704      *
035600000704      * PADRONCINO AL
035700000704      *
035800000704     C     VIDPAA        IFNE      *BLANKS
035900000704     C     VIDPAA        ANDNE     *ZEROS
036000000704     C     VIDLIA        ANDEQ     0
036100000704     C                   SETON                                        2890
036200000704     C                   GOTO      ENDCT1
036300000704     C                   ENDIF
036400000704      *
036500000704      * SE IMMESSO SOLO IL PADRONCINO DAL CONTROLLO LA SUA ESISTENZA
036600000704      *
036700000704     C     VIDPAA        IFEQ      *BLANKS
036800000704     C     VIDPAA        OREQ      *ZEROS
036900021202     C     kapd          CHAIN     fiapd000                           30
037000000704      *
037100000704     C     *IN30         IFEQ      *ON
037200000704     C     *IN30         OREQ      *OFF
037300000704     C     APDATB        ANDNE     ' '
037400000704     C                   SETON                                        2490
037500000704     C                   GOTO      ENDCT1
037600000704     C                   ENDIF
037700000705      *
037800000705     C     *IN30         IFEQ      *ON
037900000705     C     *IN30         OREQ      *OFF
038000000705     C     APDPDD        ANDEQ     'S'
038100020524     C     VIDSIM        andne     'S'
038200000705     C                   SETON                                        2490
038300000705     C                   GOTO      ENDCT1
038400000705     C                   ENDIF
038500000704     C                   MOVEL     APDRSC        VIDDEP
038600000704      *
038700000704     C                   ELSE
038800000704      *
038900000704      * CONTROLLO PADRONCINO AL
039000000704      *
039100000704     C                   TESTN                   VIDPAA               35
039200000704      *
039300000704     C     *IN35         IFEQ      *ON
039400000704     C                   MOVE      VIDPAA        W001              1
039500000704      * CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
039600000704     C     W001          IFLT      '0'
039700000704     C                   SETOFF                                       35
039800000704     C                   ENDIF
039900000704     C                   ENDIF
040000000704     C*
040100000704     C  N35              SETON                                        2890
040200000704     C  N35              GOTO      ENDCT1
040300000704      *
040400000704      * PADRONCINO AL  <  PADRONCINO DAL
040500000704      *
040600000704     C     PARCPA        IFLT      PARCPD
040700000704     C                   SETON                                        2590
040800000704     C                   GOTO      ENDCT1
040900000704     C                   ENDIF
041000000704     C                   ENDIF
041100000704      *
041200001124      * D A T A    D I S T I N T E / R I T I R I    D A L
041300000705      *
041400000705     C                   MOVE      VIDDTR        G02DAT
041500000705     C                   MOVEL     *BLANK        G02ERR
041600000705     C                   CALL      'XSRDA8'
041700000705     C                   PARM                    WLBDAT
041800000705     C     G02ERR        IFEQ      '1'
041900000705     C                   SETON                                        2690
042000000705     C                   GOTO      ENDCT1
042100000705     C                   ENDIF
042200000705     C                   Z-ADD     G02INV        PARDTR
042300000705     C                   Z-ADD     G02DAT        VIDDTR
042400001124      *
042500001124      * D A T A    D I S T I N T E / R I T I R I    A L
042600001124      *
042700001124     C     VIDDTA        IFNE      0
042800001124     C                   MOVE      VIDDTA        G02DAT
042900001124     C                   MOVEL     *BLANK        G02ERR
043000001124     C                   CALL      'XSRDA8'
043100001124     C                   PARM                    WLBDAT
043200001124     C     G02ERR        IFEQ      '1'
043300001124     C                   SETON                                        2090
043400001124     C                   GOTO      ENDCT1
043500001124     C                   ENDIF
043600001124     C                   Z-ADD     G02INV        PARDTA
043700001124     C                   Z-ADD     G02DAT        VIDDTA
043800001124     C     PARDTA        IFLT      PARDTR
043900001124     C                   SETON                                        2090
044000001124     C                   GOTO      ENDCT1
044100001124     C                   ENDIF
044200001124     C                   ELSE
044300001124     C                   MOVE      VIDDTR        VIDDTA
044400001124     C                   MOVE      PARDTR        PARDTA
044500001124     C                   ENDIF
044600001124      *
044700000705      * C O N T R O L L O   R A N G E   P A D R O N C I N I
044800000705      *
044900000705     C     VIDPAA        IFEQ      *BLANKS
045000000705     C     VIDPAA        OREQ      *ALL'0'
045100000705     C                   MOVE      PARCPD        PARCPW
045200000705     C                   ELSE
045300000705     C                   MOVE      PARCPA        PARCPW
045400000705     C                   ENDIF
045500000705      *
045600000705     C                   SETON                                        29
045700000705      *
045800000705      * LEGGO L'ANAGRAFICO PADRONCINI E VERIFICO SE TUTTI I PADRONCINI
045900000705      * COMPRESI NEL RANGE POSSONO ESSERE SVALORIZZATI ALTRIMENTI
046000000719      * EMETTO MESSAGGIONE A VIDEO
046100000705      *
046200021202     C     kapd          SETLL     fiapd01L
046300000705     C                   DO        *HIVAL
046400021202     C     apdtip        READe     fiapd01L                               35
046500000705     C   35              LEAVE
046600000705     C     APDPDR        IFGT      PARCPW
046700000705     C                   LEAVE
046800000705     C                   ENDIF
046900000705     C     APDATB        IFNE      ' '
047000000705     C     APDPDD        OREQ      'S'
047100020524     C     VIDSIM        andne     'S'
047200000705     C                   ITER
047300000705     C                   ENDIF
047400000705      *
047500000705     C                   SETOFF                                       29
047600000705      *
047700001124      * PER PADRONCINO E DATA DISTINTA DAL CI DEVE ESSERE ALMENO
047800000705      * UN RECORD IN FTT CONFERMATO, CON NDC > 0 TSR = 'C' O 'R'
047900000719      *
048000000705     C                   MOVE      APDPDR        KPDR
048100000705     C                   Z-ADD     PARDTR        KDDC
048200000719     C                   MOVE      'C'           KFVL
048300000705     C                   SETON                                        21
048400000705      *
048500011114     C   38KFTT          SETLL     fifst02L
048600011114     C  N38KFTT          SETLL     fiftt02L
048700000705     C                   DO        *HIVAL
048800011114     C   38KFTT          READE     fifst02L                               30
048900011114     C  N38KFTT          READE     fiftt02L                               30
049000000705     C   30              LEAVE
049100000705     C     FTTNDC        IFGT      0
049200000705     C     FTTTSR        IFEQ      'C'
049300000705     C     FTTTSR        OREQ      'R'
049400000705     C                   SETOFF                                       21        RCD VALIDO
049500000705     C                   ENDIF
049600000705     C                   ENDIF
049700000705     C                   ENDDO
049800000719      *
049900000719      * SE HO ERRORE MA ..
050000000719      * .. NON HO TROVATO ALCUN RECORD SPENGO L'ERRORE
050100000719      *
050200000719     C     *IN21         IFEQ      *ON
050300000719     C                   SETOFF                                       21
050400000719     C     1             DO        3             WNR               2 0
050500000719     C     WNR           IFEQ      1
050600000719     C                   MOVE      ' '           KFVL
050700000719     C                   ENDIF
050800000719     C     WNR           IFEQ      2
050900000719     C                   MOVE      'V'           KFVL
051000000719     C                   ENDIF
051100000719     C     WNR           IFEQ      3
051200000719     C                   MOVE      'B'           KFVL
051300000719     C                   ENDIF
051400011114     C   38KFTT          SETLL     fifst02L
051500011114     C  N38KFTT          SETLL     fiftt02L
051600000719     C                   DO        *HIVAL
051700011114     C   38KFTT          READE     fifst02L                               30
051800011114     C  N38KFTT          READE     fiftt02L                               30
051900000719     C   30              LEAVE
052000000719     C                   SETON                                        21        RCD NON VALIDO
052100000719     C                   ENDDO
052200000719     C                   ENDDO
052300000719     C                   ENDIF
052400000719      *
052500000719      * DO ERRORE
052600000705      *
052700000705     C     *IN21         IFEQ      *ON
052800000705     C                   Z-ADD     VIDDTR        VIDDT2
052900000705     C                   MOVEL     KPDR          VIDPA2
053000000705     C                   SETON                                        90
053100000705     C                   GOTO      ENDCT1
053200000705     C                   ENDIF
053300000705      *
053400000705     C                   ENDDO
053500000705      *
053600000705      * SE NON HO TROVATO NEANCHE UN PADRONCINO VALIDO DO ERRORE
053700000705      *
053800000705     C     *IN29         IFEQ      *ON
053900000705     C                   SETON                                        90
054000000705     C                   GOTO      ENDCT1
054100000705     C                   ENDIF
054200000705      *
054300000704     C     ENDCT1        ENDSR

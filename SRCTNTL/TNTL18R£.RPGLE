000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* STAMPA TRAINI - RICHIESTA PARAMETRI
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL18D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL18DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100060322     Ftntlz01l  IF   E           K DISK
001200110609     Ftnpsc01l  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI, COMANDI E FUNZIONI
001800000000     D*-------------------
001900110609     D MSG             S             78    DIM(21) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002200000000     D*-------------------
002300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002400000000     D*-------------------
002500000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002600000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002700000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002800000000     D*-------------------
002900000000     D* TABELLA DESCRIZIONI A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003200000000     D*-------------------
003300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003400000000     D*-------------------
003500000000     D KEYS            S              1    DIM(40)
003600000000     D*-------------------
003700000000     D* FILIALI
003800000000     D*-------------------
003900971125     D ORG             S              3  0 DIM(500)                             *CODICE
004000971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
004100000000     D*------------------------------------------------------------------------*
004200000000     D* INPUT
004300000000     D*------------------------------------------------------------------------*
004400000000     D*---------------------
004500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004600000000     D*---------------------
004700000000     D DSFUN         E DS
004800000000     D  FUNRIG                 1     40
004900000000     D*---------------------
005000000000     D* DS PER GESTIONE TASTI FUNZIONALI
005100000000     D*---------------------
005200000000     D DSKEYS        E DS
005300000000     D  KEYRIG                 1     40
005400000000     D*---------------------
005500000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005600000000     D*---------------------
005700000000     D                SDS
005800000000     D  USER                 254    263
005900000000     D*---------------------
006000000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006100000000     D*---------------------
006200000000     D DEVDS1          DS
006300000000     D  KEY                  369    369
006400000000     D  CURSOR               370    371
006500000000     D*---------------------
006600000000     D* ARCHITETTURA
006700000000     D*---------------------
006800000000     D KPJBA         E DS
006900000000     D*---------------------
007000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007100000000     D*---------------------
007200000000     D DSMSG         E DS
007300000000     D*---------------------
007400000000     D* DS ESTERNA DI PROCEDURA
007500000000     D*---------------------
007600000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007700020715
007800020715     d DdatiUte      e ds
007900020715     d AzuteDs       e ds                  ExtName(Azute00f)
008000020715     d Tibs34Ds      e ds                  Inz
008100110609     d TNTLP0DS      e ds                  Inz
008200020715
008300960208     D*---------------------
008400960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
008500960208     D*---------------------
008600960208     D OG148         E DS
008700000000     D*---------------------
008800000000     D* DS PARAMETRI DI LANCIO
008900000000     D*------------------
009000000000     D PARAM           DS                  INZ
009100960118     D  PARORD                 1      1
009200960118     D  PARDDE                 2      9  0
009300960118     D  PARFIL                10     12  0
009400960115     D  PARGIO                13     13
009500961021     D  PARTR1                14     20  0
009600961021     D  PARTR2                21     27  0
009700060322     D  PARpdr                28     34  0
009800060323     D  PARdda                35     42  0
009900060323     D  PARbis                43     43
010000110527     D  PARTpsc               44     44
010100110609     D  PARpsc                45     54
010200060322     D*****
010300060322     D* PASSAGGIO DATI ALLA RICERCA TRAZIONISTI  - TRUL16R -
010400060322     D*****
010500060322     D PARAM3          DS
010600060322     D* TIPOLOGIA T_=TRAZIONISTA
010700060322     D  PA3TIP                 1      2
010800060322     D* RAGIONE SOCIALE TRAZIONISTA
010900060322     D  PA3RSC                 3     37
011000060322     D* S.I. TRAZIONISTA
011100060322     D  PA3CSF                38     40
011200060322     D* CODICE TRAZIONISTA
011300060322     D  PA3PDR                41     47  0
011400060322     D* PA3FLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
011500060322     D*                  ALFABETICA RICHIESTA
011600060322     D  PA3FLG                48     48
011700000000     D*---------------------
011800000000     D* DS CONTROLLO DATA (8)
011900000000     D*------------------
012000000000     D WLBDA8          DS                  INZ
012100000000     D  G08DAT                 1      8  0
012200000000     D  G08INV                 9     16  0
012300000000     D  G08ERR                17     17
012400000000     D  G08TGI                18     22  0
012500110609     d
012600110609     d  Indx           s              3  0
012700000000     C*------------------------------------------------------------------------*
012800000000     C* MAIN LINE
012900000000     C*------------------------------------------------------------------------*
013000000000     C*
013100000000     C* CICLO FINO A CHE:
013200000000DO  1C     $FINE         DOUEQ     '1'
013300000000     C*
013400000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013500000000     C*
013600000000     C* CONTROLLA GESTIONE VIDEO
013700000000IF  2C     $GES          IFEQ      'V1'
013800000000     C                   EXSR      GESV1                                        *PARAMETRI
013900000000E   2C                   ENDIF
014000000000     C*
014100000000E   1C                   ENDDO
014200000000     C* FINE PGM
014300000000IF  1C     $F12          IFEQ      '1'
014400000000     C                   MOVEL     '1'           D00F12
014500000000E   1C                   ENDIF
014600000000     C*
014700000000     C                   MOVEL     DS00          KPJBU
014800000000     C                   SETON                                        LR
014900000000     C*------------------------------------------------------------------------*
015000000000     C* GESV1 - GESTIONE VIDEO UNO
015100000000     C*------------------------------------------------------------------------*
015200000000     C     GESV1         BEGSR
015300000000     C*
015400000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
015500000000     C                   EXSR      CARV1                                        *CARICA VIDEO
015600000000E   1C                   ENDIF
015700000000     C*
015800000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
015900000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
016000000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
016100000000E   1C                   ENDIF
016200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
016300000000     C*
016400000000     C                   EXFMT     TL18DV1                                      *PARAMETRI
016500000000     C*
016600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
016700000000     C*
016800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
016900000000SL  2C                   SELECT
017000000000     C     KEY           WHENEQ    ENTER
017100000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
017200000000     C     KEY           WHENEQ    F6
017300000000     C                   EXSR      F06GES                                       *CONFERMA
017400000000     C     KEY           WHENEQ    F22
017500000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
017600000000     C                   OTHER
017700000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
017800000000E   2C                   ENDSL
017900000000E   1C                   ENDIF
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* CARV1 - CARICA VIDEO UNO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     CARV1         BEGSR
018600000000     C*
018700000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
018800000000     C*
018900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
019000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
019100000000     C*
019200000000     C                   WRITE     TL18DVT                                      *TESTATA
019300000000     C                   WRITE     TL18DVF                                      *FUNZIONI
019400000000     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
019800000000     C*------------------------------------------------------------------------*
019900000000     C     ENTV1         BEGSR
020000000000     C*
020100000000     C* CONTROLLA I CAMPI IMMESSI
020200000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
020700000000     C*------------------------------------------------------------------------*
020800000000     C     F06GES        BEGSR
020900000000     C*
021000000000     C* CONTROLLA I CAMPI IMMESSI
021100000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
021200000000     C*
021300000000     C* NON CI SONO ERRORI
021400000000IF  1C     $ERR          IFEQ      '0'
021500000000     C*
021600000000     C* LANCIA IL PROGRAMMA DI STAMPA
021700960118     C                   MOVEL     V1CORD        PARORD                         *ORDINAMENTO
021800960112     C                   Z-ADD     V1CFIL        PARFIL                         *FILIALE
021900960118     C                   Z-ADD     DDEV1C        PARDDE                         *DATA DECORRENZA
022000060323     C                   Z-ADD     DDAV1C        PARDDA                         *DATA DECORRENZA
022100960115     C                   MOVEL     V1CGIO        PARGIO                         *GIORNO DI PARTENZA
022200961021     C                   Z-ADD     V1CTR1        PARTR1                         *NUMERO TRAINO DAL
022300961021     C                   Z-ADD     V1CTR2        PARTR2                         *NUMERO TRAINO AL
022400060322     c                   if        v1cpdr <= *zeros
022500060322     c                   move      *zeros        parpdr
022600060322     c                   else
022700060322     C                   move      V1Cpdr        PARpdr                         *NUMERO TRAINO AL
022800060322     c                   endif
022900060323     c                   movel     v1cbis        parbis
023000110527     c                   movel     v1ctpsc       partpsc
023100110609     c                   movel     v1cpsc        parpsc
023200000000     C                   MOVEL     'TL19'        KCOAZ
023300000000     C                   MOVEL     PARAM         KPJBU
023400000000     C                   CALL      'BCH10'
023500000000     C                   PARM                    KPJBA
023600000000     C*
023700960205     C                   MOVEL     MSG(20)       $MSG                           *STAMPA LANCIATA
023800960205     C                   MOVEL     MSG(20)       D00MSG                         *STAMPA LANCIATA
023900000000E   1C                   ENDIF
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
024400000000     C*------------------------------------------------------------------------*
024500000000     C     F22GES        BEGSR
024600000000     C*
024700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
024800000000     C                   Z-ADD     80            LENGH            15 5
024900000000     C                   CALL      'QCAEXEC'
025000000000     C                   PARM                    CMD(1)
025100000000     C                   PARM                    LENGH
025200000000     C*
025300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025800000000     C*------------------------------------------------------------------------*
025900960112     C     DECODI        BEGSR
026000000000     C*
026100000000     C* REIMPOSTA INDICATORI DI ERRORE
026200000000     C                   SETOFF                                       606162
026300961021     C                   SETOFF                                       636465
026400110609     C                   SETOFF                                       666768
026500960112     C*--------------------
026600960118     C* ORDINAMENTO
026700960112     C*--------------------
026800960112IF  1C     $ERR          IFEQ      '0'
026900960118IF  2C     V1CORD        IFEQ      '1'
027000960118     C     V1CORD        OREQ      '2'
027100960118     C     V1CORD        OREQ      '3'
027200960118     C     V1CORD        OREQ      '4'
027300960112X   2C                   ELSE
027400960112     C                   SETON                                        60
027500960112     C                   MOVEL     '1'           $ERR
027600960118     C                   MOVEL     MSG(7)        $MSG
027700960112     C                   EXSR      CARMSG
027800960112E   2C                   ENDIF
027900960112E   1C                   ENDIF
028000060323     C*--------------------
028100060323     C* Data dal
028200060323     C*--------------------
028300060323     C                   Z-ADD     *ZEROS        DDAV1C
028400060323IF  1C     $ERR          IFEQ      '0'
028500060323IF  2C     V1CDDA        IFGT      *ZEROS
028600060323     C                   Z-ADD     V1CDDA        WDA8
028700060323     C                   Z-ADD     *ZEROS        DA8W
028800060323     C                   EXSR      CHKDAT
028900060323     C                   Z-ADD     WDA8          V1CDDA
029000060323     C                   Z-ADD     DA8W          DDAV1C
029100060323IF  3C     $ERR          IFEQ      '1'
029200060323     C                   SETON                                        67
029300060323     C                   MOVEL     MSG(8)        $MSG
029400060323     C                   EXSR      CARMSG
029500060323E   3C                   ENDIF
029600060323X   2C                   ELSE
029700060323     C                   SETON                                        67
029800060323     C                   MOVEL     MSG(16)       $MSG
029900060323     C                   EXSR      CARMSG
030000060323E   2C                   ENDIF
030100060323E   1C                   ENDIF
030200960112     C*--------------------
030300060323     C* Data al
030400000000     C*--------------------
030500000000     C                   Z-ADD     *ZEROS        DDEV1C
030600000000IF  1C     $ERR          IFEQ      '0'
030700000000IF  2C     V1CDDE        IFGT      *ZEROS
030800000000     C                   Z-ADD     V1CDDE        WDA8
030900000000     C                   Z-ADD     *ZEROS        DA8W
031000000000     C                   EXSR      CHKDAT
031100000000     C                   Z-ADD     WDA8          V1CDDE
031200000000     C                   Z-ADD     DA8W          DDEV1C
031300000000IF  3C     $ERR          IFEQ      '1'
031400960118     C                   SETON                                        61
031500960118     C                   MOVEL     MSG(8)        $MSG
031600000000     C                   EXSR      CARMSG
031700000000E   3C                   ENDIF
031800040810X   2C                   ELSE
031900040810     C                   SETON                                        61
032000040810     C                   MOVEL     MSG(16)       $MSG
032100040810     C                   EXSR      CARMSG
032200000000E   2C                   ENDIF
032300000000E   1C                   ENDIF
032400060323     C*--------------------
032500060323     C* Data AL < di data Dal
032600060323     C*--------------------
032700060323IF  1C     $ERR          IFEQ      '0'
032800060323     C     DDEV1C        ANDLT     DDAV1C
032900060323     C                   SETON                                        6167
033000060323     C                   MOVEL     '1'           $ERR
033100060323     C                   MOVEL     MSG(18)       $MSG
033200060323     C                   EXSR      CARMSG
033300060323     C                   ENDIF
033400960118     C*--------------------
033500960118     C* FILIALE
033600960118     C*--------------------
033700960118     C                   MOVEL     *BLANKS       V1DFIL
033800960118IF  1C     $ERR          IFEQ      '0'
033900960118IF  2C     V1CFIL        IFGT      *ZEROS
034000960118     C                   Z-ADD     V1CFIL        WFIL
034100960118     C                   MOVEL     *BLANKS       WDFIL
034200960118     C                   EXSR      CHKFIL
034300960118     C                   MOVEL     WDFIL         V1DFIL
034400960118IF  3C     $ERR          IFEQ      '1'
034500960118     C                   SETON                                        62
034600960118     C                   MOVEL     MSG(9)        $MSG
034700960118     C                   EXSR      CARMSG
034800960118E   3C                   ENDIF
034900960118E   2C                   ENDIF
035000960118E   1C                   ENDIF
035100960115     C*--------------------
035200960118     C* GIORNO DI SELEZIONE
035300960115     C*--------------------
035400960115IF  1C     $ERR          IFEQ      '0'
035500960115IF  2C     V1CGIO        IFEQ      *BLANKS
035600960115     C     V1CGIO        OREQ      '1'
035700960115     C     V1CGIO        OREQ      '2'
035800960115     C     V1CGIO        OREQ      '3'
035900960115     C     V1CGIO        OREQ      '4'
036000960115     C     V1CGIO        OREQ      '5'
036100960115     C     V1CGIO        OREQ      '6'
036200960115     C     V1CGIO        OREQ      '7'
036300960115X   2C                   ELSE
036400960115     C                   SETON                                        63
036500960115     C                   MOVEL     '1'           $ERR
036600960115     C                   MOVEL     MSG(10)       $MSG
036700960115     C                   EXSR      CARMSG
036800960115E   2C                   ENDIF
036900960115E   1C                   ENDIF
037000961021     C*--------------------
037100961021     C* NUMERO TRAINO DAL-AL
037200961021     C*--------------------
037300961021IF  1C     $ERR          IFEQ      '0'
037400961021IF  2C     V1CTR1        IFGT      *ZEROS
037500961021     C     V1CTR2        ANDGT     *ZEROS
037600961021IF  3C     V1CTR1        IFGT      V1CTR2
037700961021     C                   SETON                                        6465
037800961021     C                   MOVEL     '1'           $ERR
037900961021     C                   MOVEL     MSG(15)       $MSG
038000961021     C                   EXSR      CARMSG
038100961021E   3C                   ENDIF
038200961021E   2C                   ENDIF
038300961021E   1C                   ENDIF
038400060322     C*--------------------
038500060322     C* Trazionista
038600060322     C*--------------------
038700060322IF  1C     $ERR          IFEQ      '0'
038800060322     c                   clear                   v1dpdr
038900060322IF  2C     V1Cpdr        ifne      *blanks
039000060322    1C     V1CPDR        ANDNE     *ZEROS
039100060322     C* RICERCA
039200060322     C     '?'           SCAN      V1CPDR                                 90
039300060322    2C     *IN90         IFEQ      *ON
039400060322     C                   EVAL      PA3TIP = 'T '                                PAR-1
039500060322     C                   clear                   PA3RSC                         PAR-2
039600060322     C                   CLEAR                   PA3CSF                         PAR-3
039700060322     C                   CLEAR                   PA3PDR                         PAR-4
039800060322     C                   CLEAR                   PA3FLG                         PAR-5
039900060322     C                   MOVEL     PARAM3        KPJBU
040000060322     C                   CALL      'TRUL16R'
040100060322     C                   PARM                    KPJBA
040200060322     C                   MOVEL     KPJBU         PARAM3
040300060322     C                   MOVEL     PA3PDR        V1CPDR                         CODICE
040400060322     C                   MOVEL     PA3RSC        V1DPDR                         RAG.SOCIALE
040500060322     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
040600060322    2C                   ENDIF
040700060322     C*
040800060322     C                   TESTN                   V1CPDR               31
040900060322     C                   MOVE      V1CPDR        W001A             1
041000060322    2C     *IN31         IFEQ      *OFF
041100060322     C     W001A         ORLT      '0'
041200060322     C                   MOVEL     '1'           $ERR
041300060322     C                   MOVEL     MSG(17)       $MSG
041400060322     C                   SETON                                        66
041500060322     c                   exsr      carmsg
041600060322   X2C                   ELSE
041700060322     C* CONTROLLO SULL'ANAGRAFICA
041800060322     C                   move      v1cpdr        KTLPDR
041900060322     C     KTNTLZ        CHAIN     TNTLZ01L                           31
042000060322    3C     *IN31         IFEQ      *ON
042100060322     C     TLZATB        ORNE      ' '
042200060322     C                   MOVEL     '1'           $ERR
042300060322     C                   MOVEL     MSG(17)       $MSG
042400060322     C                   SETON                                        66
042500060322     c                   exsr      carmsg
042600060322   X3C                   ELSE
042700060322     C                   MOVEL     TLZRSC        V1DPDR
042800060322    3C                   ENDIF
042900060322    2C                   ENDIF
043000060322E   2C                   ENDIF
043100060322E   1C                   ENDIF
043200110609     C*--------------------
043300110609     C* codice punto di scambio
043400110609     C*--------------------
043500110609     c
043600110609IF  1C                   IF        $ERR ='0'
043700110609     c                   clear                   desloc
043800110609     c                   clear                   desind
043900110609     c                   clear                   descap
044000110609     c                   clear                   desnot
044100110609IF 1aC                   IF        v1cpsc<>*blanks
044200110609     C                   EVAL      INDX=%scan('?':v1cpsc)
044300110609     c
044400110609    2c                   if        indx>0
044500110609     c* imposta la ds di procedura
044600110609     c                   CLEAR                   tntlp0ds
044700110609     c                   MOVE      '01'          dp0op0                         *livello procedura
044800110609     c                   MOVEL     *blanks       dp0op1                         *livello programma
044900110609     c                   MOVEL     '0'           dp0f03                         *premuto F03
045000110609     c                   MOVEL     '0'           dp0f12                         *premuto F12
045100110609     c                   MOVEL     '0'           dp0err                         *errore
045200110609     c                   call      'TNTLP0R'
045300110609     c                   parm                    kpjba
045400110609     c                   parm                    TNTLp0ds
045500110609     c
045600110609     c                   eval      v1cpsc=dp0psc
045700110609     c                   eval      desloc=dp0loc
045800110609     c                   eval      desnot=dp0not
045900110609     c                   if        dp0prv<>*blanks
046000110609     c                   eval      desloc=%trim(dp0loc)+'  '+dp0prv+dp0nar
046100110609     c                   else
046200110609     c                   eval      desloc=%trim(dp0loc)+' '+dp0nar
046300110609     c                   endif
046400110609     c                   eval      desind=dp0ind
046500110609     c                   eval      descap=dp0cap
046600110609     C                   SETON                                        68
046700110609     C                   MOVEL     '1'           $ERR
046800110609     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
046900110609     C                   LEAVEsr
047000110609    2c                   endif
047100110609     c
047200110609     c     v1cpsc        chain     tnpsc01l
047300110609    2c                   if        not %found(tnpsc01l)
047400110609     C                   MOVEL     '1'           $ERR
047500110609     C                   MOVEL     MSG(19)       $MSG
047600110609     C                   EXSR      CARMSG
047700110609     C                   SETON                                        68
047800110609     C                   LEAVEsr
047900110609E   2C                   ENDIF
048000110609     c
048100110609     c                   eval      desloc=pscloc
048200110609     c                   eval      desnot=pscnot
048300110609    2c                   if        pscprv<>*blanks
048400110609     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
048500110609     c                   else
048600110609     c                   eval      desloc=%trim(desloc)+' '+pscnar
048700110609    2c                   endif
048800110609     c                   eval      desind=pscind
048900110609     c                   eval      descap=psccap
049000110609   1ac                   endif
049100110609    1c                   endif
049200110609     c
049300110609     c* Se c'e'  codice di scambiod evo per forza mettere S nella selezione dei traini con scambio
049400110609IF  1C     $ERR          IFEQ      '0'
049500110609    2c                   if        v1cpsc<>*blanks and v1ctpsc<>'S'
049600110609     C                   SETON                                        68
049700110609     C                   MOVEL     '1'           $ERR
049800110609     C                   MOVEL     MSG(21)       $MSG
049900110609     C                   EXSR      CARMSG
050000110609E   2C                   ENDIF
050100110609E   1C                   ENDIF
050200110609
050300960118     C*--------------------
050400960118     C* PRIMO ORDINAMENTO
050500960118     C*--------------------
050600960118IF  1C     $ERR          IFEQ      '0'
050700960118IF  2C     V1CORD        IFEQ      '1'
050800960118IF  3C     V1CFIL        IFGT      *ZEROS
050900960118     C     V1CGIO        ORNE      *BLANKS
051000960118     C                   SETON                                        60
051100960118     C                   MOVEL     '1'           $ERR
051200960118     C                   MOVEL     MSG(11)       $MSG
051300960118     C                   EXSR      CARMSG
051400960118E   2C                   ENDIF
051500960118E   2C                   ENDIF
051600960118E   1C                   ENDIF
051700960118     C*--------------------
051800960118     C* SECONDO ORDINAMENTO
051900960118     C*--------------------
052000960118IF  1C     $ERR          IFEQ      '0'
052100960118IF  2C     V1CORD        IFEQ      '2'
052200960213IF  3C     V1CGIO        IFNE      *BLANKS
052300960118     C                   SETON                                        60
052400960118     C                   MOVEL     '1'           $ERR
052500960118     C                   MOVEL     MSG(12)       $MSG
052600960118     C                   EXSR      CARMSG
052700960213E   3C                   ENDIF
052800960118E   2C                   ENDIF
052900960118E   1C                   ENDIF
053000960118     C*--------------------
053100960118     C* TERZO ORDINAMENTO
053200960118     C*--------------------
053300960118IF  1C     $ERR          IFEQ      '0'
053400960118IF  2C     V1CORD        IFEQ      '3'
053500960118IF  3C     V1CFIL        IFEQ      *ZEROS
053600960118     C                   SETON                                        60
053700960118     C                   MOVEL     '1'           $ERR
053800960118     C                   MOVEL     MSG(13)       $MSG
053900960118     C                   EXSR      CARMSG
054000960213E   3C                   ENDIF
054100960118E   2C                   ENDIF
054200960118E   1C                   ENDIF
054300960118     C*--------------------
054400960118     C* QUARTO ORDINAMENTO
054500960118     C*--------------------
054600960118IF  1C     $ERR          IFEQ      '0'
054700960118IF  2C     V1CORD        IFEQ      '4'
054800960118IF  3C     V1CFIL        IFEQ      *ZEROS
054900960118     C                   SETON                                        60
055000960118     C                   MOVEL     '1'           $ERR
055100960118     C                   MOVEL     MSG(14)       $MSG
055200960118     C                   EXSR      CARMSG
055300960213E   3C                   ENDIF
055400960118E   2C                   ENDIF
055500960118E   1C                   ENDIF
055600000000     C*
055700000000     C                   ENDSR
055800960112     C*------------------------------------------------------------------------*
055900960112     C* CHKFIL - CONTROLLA LA FILIALE
056000960112     C*          INPUT  -> WFIL  - FILIALE
056100960112     C*          OUTPUT -> WDFIL - DESCRIZIONE FILIALE
056200960112     C*------------------------------------------------------------------------*
056300960112     C     CHKFIL        BEGSR
056400960112     C*
056500960116     C     *LIKE         DEFINE    V1CFIL        WFIL
056600960112     C     *LIKE         DEFINE    ORGDES        WDFIL
056700060322     C     *LIKE         DEFINE    TLZPDR        KTLPDR
056800060322     C     *LIKE         DEFINE    TLZTIP        KTLTIP
056900960112     C*
057000960112     C                   MOVEL     *BLANKS       WDFIL
057100960112     C*
057200960112     C                   Z-ADD     WFIL          KORFIL
057300960112     C     KEYORG        CHAIN     AZORG01L                           28
057400960112IF  1C     *IN28         IFEQ      *ON
057500960112     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
057600960112     C                   MOVEL     '1'           $ERR
057700960112X   1C                   ELSE
057800960208     C                   MOVEL     ORGDE8        OG148
057900960208IF  2C     §OGLFV        IFEQ      'N'                                          *IN GESTIONE FV
058000960208     C                   MOVEL     '1'           $ERR
058100960208X   2C                   ELSE
058200960112     C                   MOVEL     ORGDES        WDFIL                          *DESCRIZIONE
058300960208E   2C                   ENDIF
058400960208E   1C                   ENDIF
058500960112     C*
058600960112     C                   ENDSR
058700000000     C*------------------------------------------------------------------------*
058800000000     C* CHKDAT - CONTROLLA LA DATA
058900000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
059000000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
059100000000     C*------------------------------------------------------------------------*
059200000000     C     CHKDAT        BEGSR
059300000000     C*
059400000000     C     *LIKE         DEFINE    V1CDDE        WDA8
059500000000     C     *LIKE         DEFINE    V1CDDE        DA8W
059600000000     C*
059700000000     C                   Z-ADD     *ZEROS        DA8W
059800000000     C*
059900000000     C                   Z-ADD     WDA8          G08DAT
060000000000     C                   Z-ADD     *ZEROS        G08INV
060100000000     C                   MOVEL     '0'           G08ERR
060200000000     C                   CALL      'XSRDA8'
060300000000     C                   PARM                    WLBDA8
060400000000     C                   Z-ADD     G08DAT        WDA8
060500000000IF  1C     G08ERR        IFEQ      '1'
060600000000     C                   MOVEL     '1'           $ERR
060700000000     C                   ELSE
060800000000     C                   Z-ADD     G08INV        DA8W
060900000000E   1C                   ENDIF
061000000000     C*
061100000000     C                   ENDSR
061200000000     C*------------------------------------------------------------------------*
061300000000     C* IMPVID - IMPOSTAZIONI VIDEO
061400000000     C*------------------------------------------------------------------------*
061500000000     C     IMPVID        BEGSR
061600000000     C*
061700960112     C* CALCOLA LA DATA CORRENTE
061800960112     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
061900960112     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
062000960118     C*
062100960118     C                   Z-ADD     WNUM8         V1CDDE                         *DECORRENZA
062200060323     C                   Z-ADD     WNUM8         V1CDDa                         *DECORRENZA
062300060324     c                   movel     'E'           v1cbis
062400110527     c                   movel     'N'           v1ctpsc
062500960118     C* SE FILIALE
062600020715     c                   If        DutLpo <> 'S'
062700960118     C                   MOVEL     '3'           V1CORD                         *TERZO ORDINAMENTO
062800020715     c                   Z-Add     DutPou        V1cFil
062900960118     C* SE SEDE
063000960118X   1C                   ELSE
063100960118     C                   MOVEL     '1'           V1CORD                         *PRIMO ORDINAMENTO
063200960116E   1C                   ENDIF
063300000000     C*
063400000000     C                   ENDSR
063500000000     C*------------------------------------------------------------------------*
063600000000     C* CARTAB - CARICA LE TABELLE
063700000000     C*------------------------------------------------------------------------*
063800000000     C     CARTAB        BEGSR
063900000000     C*--------------------
064000000000     C* FILIALI
064100000000     C*--------------------
064200000000     C                   Z-ADD     *ZEROS        J                 4 0
064300000000     C     *LOVAL        SETLL     AZORG01L
064400000000     C                   READ      AZORG01L                               99
064500000000DO  1C     *IN99         DOWEQ     *OFF
064600000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
064700960208     C                   MOVEL     ORGDE8        OG148
064800960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
064900000000     C                   ADD       1             J
065000000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
065100000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
065200960208E   3C                   ENDIF
065300960208E   2C                   ENDIF
065400000000     C                   READ      AZORG01L                               99
065500000000E   1C                   ENDDO
065600960112     C*
065700000000     C                   ENDSR
065800000000     C*------------------------------------------------------------------------*
065900000000     C* *INZSR - ROUTINE INIZIALE
066000000000     C*------------------------------------------------------------------------*
066100000000     C     *INZSR        BEGSR
066200000000     C*--------------------
066300000000     C* RICEVIMENTO PARAMETRI
066400000000     C*--------------------
066500000000     C     *ENTRY        PLIST
066600000000     C                   PARM                    KPJBA
066700000211     C                   MOVEL     KPJBU         DS00
066800960112     C*--------------------
066900960112     C* VARIABILI RIFERITE AL DATA BASE
067000960112     C*--------------------
067100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
067200000000     C*--------------------
067300000000     C* VARIABILI  N O N  RIFERITE AL DATA BASE
067400000000     C*--------------------
067500000000     C     *LIKE         DEFINE    V1CDDE        DDEV1C
067600060323     C     *LIKE         DEFINE    V1CDDA        DDAV1C
067700000000     C*--------------------
067800000000     C* CHIAVI DI LETTURA
067900000000     C*--------------------
068000000000     C* LETTURA AZORG01L - COMPLETA
068100000000     C     KEYORG        KLIST
068200000000     C                   KFLD                    KORFIL                         *FILIALE
068300060322     C     KTNTLZ        KLIST
068400060322     C                   KFLD                    KTLTIP
068500060322     C                   KFLD                    KTLPDR
068600060322     C                   EVAL      KTLTIP = 'T '
068700000000     C*--------------------
068800000000     C* VARIABILI DI CONTROLLO
068900000000     C*--------------------
069000000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
069100000000     C***
069200000000     C* IMPOSTAZIONI VIDEO
069300000000     C***
069400000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
069500000000     C                   MOVE      D00OP0        INTGES            2
069600000000IF  1C     INTGES        IFEQ      '02'
069700000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
069800000000X   1C                   ELSE
069900000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
070000000000E   1C                   ENDIF
070100000000     C*
070200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
070300000000     C                   EXSR      INZSTD
070400000000     C*
070500000000     C* CARICA LE TABELLE
070600000000     C                   EXSR      CARTAB
070700000000     C*
070800000000     C* IMPOSTAZIONI VIDEO
070900000000     C                   EXSR      IMPVID
071000000000     C*
071100000000     C* EMETTE VIDEATA
071200000000     C                   EXSR      CARV1
071300000000     C*
071400000000     C                   ENDSR
071500000000     C*------------------------------------------------------------------------*
071600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
071900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
072000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
072100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072300000000     C*------------------------------------------------------------------------*
072400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
072500000000     C*------------------------------------------------------------------------*
072600000000     C     INZSTD        BEGSR
072700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
072800000000     C                   CALL      'XKEYS'
072900000000     C                   PARM                    DSKEYS
073000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
073100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
073200000000     C     XMSGPA        PLIST
073300000000     C                   PARM                    DSMSMS
073400000000     C                   PARM                    DSMSPG
073500000000     C                   PARM                    DSMSKE
073600000000     C                   PARM                    DSMSER
073700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
073800000000     C                   MOVEL     'TNTL18R'     DSMSPG                         *NOME PGM CHIAMANTE
073900000000     C                   MOVEL     'TNTL18R'     VMCQUE                         *CODA MESSAGGI DESTI
074000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
074100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
074200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
074300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
074400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
074500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
074600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
074700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
074800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
074900000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
075000000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
075100000000     C* LANCIO ROUTINE PER VERIFICARE SE
075200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
075300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
075400020715
075500020715     c     *dtaara       define    §azute        azuteds
075600020715     c     *dtaara       define    §datiute      ddatiute
075700020715
075800020715     c                   in(E)     *dtaara
075900020715     c                   if        %Error or rsut = *blanks
076000020715     c                   clear                   tibs34ds
076100020715     c                   call      'TIBS34R'
076200020715     c                   parm                    Tibs34Ds
076300020715     c                   in        *dtaara
076400020715     c                   endif
076500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
076600000000     C                   BITOFF    '01234567'    POSMI1            1
076700000000     C                   BITOFF    '01234567'    POSMI2            1
076800000000     C                   BITOFF    '01234567'    POSMA1            1
076900000000     C                   BITOFF    '01234567'    POSMA2            1
077000000000     C                   BITON     '34'          POSMI1            1            *18
077100000000     C                   BITON     '7'           POSMI2            1            *01
077200000000     C                   BITON     '34'          POSMA1            1            *18
077300000000     C                   BITON     '0'           POSMA2            1            *80
077400000000     C                   MOVE      POSMI2        POSMIN            2
077500000000     C                   MOVEL     POSMI1        POSMIN
077600000000     C                   MOVE      POSMA2        POSMAX            2
077700000000     C                   MOVEL     POSMA1        POSMAX
077800000000     C                   ENDSR
077900000000     C*------------------------------------------------------------------------*
078000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
078100000000     C*------------------------------------------------------------------------*
078200000000     C     FUNSTD        BEGSR
078300000000     C                   SELECT
078400000000     C     KEY           WHENEQ    F3
078500000000     C                   EXSR      F03STD
078600000000     C     KEY           WHENEQ    F12
078700000000     C                   EXSR      F12STD
078800000000     C     KEY           WHENEQ    F24
078900000000     C                   EXSR      F24STD
079000000000     C                   OTHER
079100000000     C                   MOVEL     MSG(2)        $MSG
079200000000     C                   EXSR      CARMSG
079300000000     C                   ENDSL
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
079700000000     C*------------------------------------------------------------------------*
079800000000     C     F03STD        BEGSR
079900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
080000000000     C                   MOVEL     '1'           $F03
080100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
080200000000     C                   MOVEL     '1'           $FINE
080300951212     C* PULISCE IL MESSAGGIO
080400951212     C                   MOVEL     *BLANKS       D00MSG
080500000000     C                   ENDSR
080600000000     C*------------------------------------------------------------------------*
080700000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
080800000000     C*------------------------------------------------------------------------*
080900000000     C     F12STD        BEGSR
081000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
081100000000     C                   MOVEL     '1'           $F12
081200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
081300000000     C                   MOVEL     '1'           $FINE
081400951212     C* PULISCE IL MESSAGGIO
081500951212     C                   MOVEL     *BLANKS       D00MSG
081600000000     C                   ENDSR
081700000000     C*------------------------------------------------------------------------*
081800000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
081900000000     C*------------------------------------------------------------------------*
082000000000     C     F24STD        BEGSR
082100000000     C*
082200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
082300000000     C                   EXSR      OUTFUN
082400000000     C*
082500000000     C                   ENDSR
082600000000     C*------------------------------------------------------------------------*
082700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
082800000000     C*          CODA MESSAGGI DEL PGM
082900000000     C*------------------------------------------------------------------------*
083000000000     C     CARMSG        BEGSR
083100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
083200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
083300000000     C     MSGULT        IFEQ      0
083400000000     C                   MOVEL     '1'           *IN40
083500000000     C                   WRITE     TL18DVK
083600000000     C                   MOVEL     '0'           *IN40
083700000000     C                   ENDIF
083800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
083900000000     C                   MOVEL     $MSG          DSMSMS
084000000000     C                   CALL      'XMSG'        XMSGPA
084100000000     C     DSMSER        IFEQ      0
084200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
084300000000     C                   ENDIF
084400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
084500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
084600000000     C                   WRITE     TL18DVM                                      *SFL MESS
084700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
084800000000     C                   MOVEL     *BLANKS       $MSG
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
085200000000     C*------------------------------------------------------------------------*
085300000000     C     OUTMSG        BEGSR
085400000000     C* VISUALIZZAZIONE MESSAGGIO
085500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
085600000000     C                   WRITE     TL18DVK                                      *CONTR MESS
085700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
085800000000     C                   Z-ADD     *ZEROS        MSGULT
085900000000     C                   ENDSR
086000000000     C*------------------------------------------------------------------------*
086100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
086200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
086300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
086400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
086500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
086600000000     C*------------------------------------------------------------------------*
086700000000     C     CARFUN        BEGSR
086800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
086900000000     C     1             DO        31            I                 5 0          --- 1 -->
087000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
087100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
087200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
087300020715     c                   If        DutLpo = 'S'
087400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
087500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
087600000000     C                   ELSE                                                   *INTERROG.
087700000000     C                   MOVEL     FUNASI        FAT(I)
087800000000     C                   ENDIF
087900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
088000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
088100000000     C                   MOVEL     FUNAFG        FAT(I)
088200000000     C                   ELSE                                                   *INTERROG.
088300000000     C                   MOVEL     FUNAFI        FAT(I)
088400000000     C                   ENDIF
088500000000     C                   ENDIF                                                  <-- 2 ---
088600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
088700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
088800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
088900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
089000000000     C                   ELSE
089100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
089200000000     C                   ENDIF
089300000000     C                   ENDIF
089400000000     C                   ENDDO                                                  <-- 1 ---
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
089800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
089900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
090000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
090100000000     C*------------------------------------------------------------------------*
090200000000     C     OUTFUN        BEGSR
090300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
090400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
090500000000     C                   ENDDO
090600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
090700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
090800000000     C     1             DO        31            I
090900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
091000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
091100000000     C                   ADD       1             FUNNAT
091200000000     C                   ENDIF
091300000000     C                   ENDDO
091400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
091500000000     C     FUNNAT        IFGT      8
091600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
091700000000     C                   ENDIF
091800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
091900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
092000000000     C                   Z-ADD     8             FUNVIS            5 0
092100000000     C     FAT(23)       IFEQ      'S'
092200000000     C                   SUB       1             FUNVIS
092300000000     C                   ENDIF
092400000000     C     FAT(24)       IFEQ      'S'
092500000000     C                   SUB       1             FUNVIS
092600000000     C                   ENDIF
092700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
092800000000     C                   Z-ADD     *ZEROS        W                 5 0
092900000000     C                   ADD       1             FUNULT
093000000000     C     FUNULT        IFGT      31
093100000000     C                   Z-ADD     1             FUNULT
093200000000     C                   ENDIF
093300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
093400000000     C     FUNULT        DO        31            I                              --- 1 -->
093500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
093600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
093700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
093800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
093900000000     C     FDE(I)        ANDNE     *BLANKS
094000000000     C     I             IFEQ      23                                           *TOLGO F23
094100000000     C     I             OREQ      24                                           *E F24
094200000000     C                   ELSE
094300000000     C                   ADD       1             W
094400000000     C                   MOVEL     FDE(I)        FUV(W)
094500000000     C                   ENDIF
094600000000     C                   ENDIF                                                  <-- 3 ---
094700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
094800000000     C                   Z-ADD     I             FUNULT
094900000000     C                   ENDIF                                                  <-- 2 ---
095000000000     C                   ENDDO                                                  <-- 1 ---
095100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
095200000000     C                   MOVEL     FUV(1)        VFD001
095300000000     C                   MOVEL     FUV(2)        VFD002
095400000000     C                   MOVEL     FUV(3)        VFD003
095500000000     C                   MOVEL     FUV(4)        VFD004
095600000000     C                   MOVEL     FUV(5)        VFD005
095700000000     C                   MOVEL     FUV(6)        VFD006
095800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
095900000000     C     FAT(24)       ANDEQ     'N'
096000000000     C                   MOVEL     FUV(7)        VFD007
096100000000     C                   MOVEL     FUV(8)        VFD008
096200000000     C                   ELSE                                                   <-- 1 -->
096300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
096400000000     C     FAT(24)       ANDEQ     'S'
096500000000     C                   MOVEL     FDE(23)       VFD007
096600000000     C                   MOVEL     FDE(24)       VFD008
096700000000     C                   ELSE                                                   <-- 2 -->
096800000000     C                   MOVEL     FUV(7)        VFD007
096900000000     C     FAT(23)       IFEQ      'S'
097000000000     C                   MOVEL     FDE(23)       VFD008
097100000000     C                   ELSE
097200000000     C                   MOVEL     FDE(24)       VFD008
097300000000     C                   ENDIF
097400000000     C                   ENDIF                                                  <-- 2 ---
097500000000     C                   ENDIF                                                  <-- 1 ---
097600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
097700000000     C                   ENDSR
097800000000     C*------------------------------------------------------------------------*
097900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
098000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
098100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
098200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
098300000000     C*------------------------------------------------------------------------*
098400000000     C     CONFUN        BEGSR
098500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
098600000000     C     1             DO        31            I                              --- 1 -->
098700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
098800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
098900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
099000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
099100000000     C                   EXSR      CARMSG
099200000000     C                   MOVEL     '1'           $ERR
099300000000     C                   ENDIF                                                  <-- 3 ---
099400000000     C                   ENDIF                                                  <-- 2 ---
099500000000     C                   ENDDO                                                  <-- 1 ---
099600000000     C                   ENDSR
099700000000     O*------------------------------------------------------------------------*
099800000000** MSG - (MESSAGGI)
099900000000TNTL18R- Tasto funzionale non valido.                                          1
100000000000TNTL18R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
100100000000TNTL18R- Opzione non valida.                                                   3
100200000000TNTL18R- Opzione abilitata ma non gestita. Avvertire CED.                      4
100300000000TNTL18R- Inizio elenco.                                                        5
100400000000TNTL18R- Fine elenco.                                                          6
100500960118TNTL18R- Richiedere un ordinamento fra quelli possibili                        7
100600960118TNTL18R- Data errata                                                           8
100700961022TNTL18R- Punto operativo errato                                                9
100800960118TNTL18R- Giorno di selezione errato                                           10
100900961022TNTL18R- Richiedere solo la decorrenza per il 1° ordinamento                  11
101000961022TNTL18R- Richiedere solo decorrenza e/o punto operativo per il 2° ordinamento 12
101100961022TNTL18R- Immettere il punto operativo per il 3° ordinamento                   13
101200961022TNTL18R- Immettere il punto operativo per il 4° ordinamento                   14
101300961021TNTL18R- Il numero traino dal deve essere minore del numero traino al         15
101400040810TNTL18R- Data obbligatoria.                                                   16
101500060322TNTL18R- Codice TRAZIONISTA obbligatorio o inesistente in anagrafica
101600060323TNTL18R- "Data Al" minore di "Data Dal"
101700110609TNTL18R- codice punti di scambio errato
101800960112TNTL18R- Stampa lanciata ....                                                 20
101900110609Se parzializza.per punti di scambio immettere "S=Si" in traino con scambio    20
102000000000** CMD  (COMANDI)
102100000000WRKSPLF
102200000000
102300000000
102400000000
102500000000
102600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
102700000000F1    -                   -    NN     NN
102800000000F2    -                   -    NN     NN
102900951213F3    -F3=Fine            -    SS     SS
103000000000F4    -                   -    NN     NN
103100000000F5    -                   -    NN     NN
103200000000F6    -F6=Conferma        -    SS     SS
103300000000F7    -                   -    NN     NN
103400000000F8    -                   -    NN     NN
103500000000F9    -                   -    NN     NN
103600000000F10   -                   -    NN     NN
103700000000F11   -                   -    NN     NN
103800951206F12   -F12=Ritorno        -    SS     SS
103900000000F13   -                   -    NN     NN
104000000000F14   -                   -    NN     NN
104100000000F15   -                   -    NN     NN
104200000000F16   -                   -    NN     NN
104300000000F17   -                   -    NN     NN
104400000000F18   -                   -    NN     NN
104500000000F19   -                   -    NN     NN
104600000000F20   -                   -    NN     NN
104700000000F21   -                   -    NN     NN
104800000000F22   -F22=Gestione stampe-    EE     EE
104900000000F23   -                   -    NN     NN
105000000000F24   -F24=Altri tasti    -    NN     NN
105100000000ENTER -                   -    SS     SS
105200000000ROLLUP-                   -    NN     NN
105300000000ROLLDN-                   -    NN     NN
105400000000HELP  -                   -    NN     NN
105500000000PRINT -                   -    NN     NN
105600000000BSPACE-                   -    NN     NN
105700000000CLEAR -                   -    NN     NN

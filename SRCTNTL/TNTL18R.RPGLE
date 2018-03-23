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
001900150123     D MSG             S             78    DIM(22) CTDATA PERRCD(1)             *MESSAGGI
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
012700150123     d  Diff           s              3  0
012800000000     C*------------------------------------------------------------------------*
012900000000     C* MAIN LINE
013000000000     C*------------------------------------------------------------------------*
013100000000     C*
013200000000     C* CICLO FINO A CHE:
013300000000DO  1C     $FINE         DOUEQ     '1'
013400000000     C*
013500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013600000000     C*
013700000000     C* CONTROLLA GESTIONE VIDEO
013800000000IF  2C     $GES          IFEQ      'V1'
013900000000     C                   EXSR      GESV1                                        *PARAMETRI
014000000000E   2C                   ENDIF
014100000000     C*
014200000000E   1C                   ENDDO
014300000000     C* FINE PGM
014400000000IF  1C     $F12          IFEQ      '1'
014500000000     C                   MOVEL     '1'           D00F12
014600000000E   1C                   ENDIF
014700000000     C*
014800000000     C                   MOVEL     DS00          KPJBU
014900000000     C                   SETON                                        LR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* GESV1 - GESTIONE VIDEO UNO
015200000000     C*------------------------------------------------------------------------*
015300000000     C     GESV1         BEGSR
015400000000     C*
015500000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
015600000000     C                   EXSR      CARV1                                        *CARICA VIDEO
015700000000E   1C                   ENDIF
015800000000     C*
015900000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
016000000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
016100000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
016200000000E   1C                   ENDIF
016300000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
016400000000     C*
016500000000     C                   EXFMT     TL18DV1                                      *PARAMETRI
016600000000     C*
016700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
016800000000     C*
016900000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
017000000000SL  2C                   SELECT
017100000000     C     KEY           WHENEQ    ENTER
017200000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
017300000000     C     KEY           WHENEQ    F6
017400000000     C                   EXSR      F06GES                                       *CONFERMA
017500000000     C     KEY           WHENEQ    F22
017600000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
017700000000     C                   OTHER
017800000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
017900000000E   2C                   ENDSL
018000000000E   1C                   ENDIF
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400000000     C* CARV1 - CARICA VIDEO UNO
018500000000     C*------------------------------------------------------------------------*
018600000000     C     CARV1         BEGSR
018700000000     C*
018800000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
018900000000     C*
019000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
019100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
019200000000     C*
019300000000     C                   WRITE     TL18DVT                                      *TESTATA
019400000000     C                   WRITE     TL18DVF                                      *FUNZIONI
019500000000     C*
019600000000     C                   ENDSR
019700000000     C*------------------------------------------------------------------------*
019800000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
019900000000     C*------------------------------------------------------------------------*
020000000000     C     ENTV1         BEGSR
020100000000     C*
020200000000     C* CONTROLLA I CAMPI IMMESSI
020300000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
020800000000     C*------------------------------------------------------------------------*
020900000000     C     F06GES        BEGSR
021000000000     C*
021100000000     C* CONTROLLA I CAMPI IMMESSI
021200000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
021300000000     C*
021400000000     C* NON CI SONO ERRORI
021500000000IF  1C     $ERR          IFEQ      '0'
021600000000     C*
021700000000     C* LANCIA IL PROGRAMMA DI STAMPA
021800960118     C                   MOVEL     V1CORD        PARORD                         *ORDINAMENTO
021900960112     C                   Z-ADD     V1CFIL        PARFIL                         *FILIALE
022000960118     C                   Z-ADD     DDEV1C        PARDDE                         *DATA DECORRENZA
022100060323     C                   Z-ADD     DDAV1C        PARDDA                         *DATA DECORRENZA
022200960115     C                   MOVEL     V1CGIO        PARGIO                         *GIORNO DI PARTENZA
022300961021     C                   Z-ADD     V1CTR1        PARTR1                         *NUMERO TRAINO DAL
022400961021     C                   Z-ADD     V1CTR2        PARTR2                         *NUMERO TRAINO AL
022500060322     c                   if        v1cpdr <= *zeros
022600060322     c                   move      *zeros        parpdr
022700060322     c                   else
022800060322     C                   move      V1Cpdr        PARpdr                         *NUMERO TRAINO AL
022900060322     c                   endif
023000060323     c                   movel     v1cbis        parbis
023100110527     c                   movel     v1ctpsc       partpsc
023200110609     c                   movel     v1cpsc        parpsc
023300000000     C                   MOVEL     'TL19'        KCOAZ
023400000000     C                   MOVEL     PARAM         KPJBU
023500000000     C                   CALL      'BCH10'
023600000000     C                   PARM                    KPJBA
023700000000     C*
023800960205     C                   MOVEL     MSG(20)       $MSG                           *STAMPA LANCIATA
023900960205     C                   MOVEL     MSG(20)       D00MSG                         *STAMPA LANCIATA
024000000000E   1C                   ENDIF
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
024500000000     C*------------------------------------------------------------------------*
024600000000     C     F22GES        BEGSR
024700000000     C*
024800000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
024900000000     C                   Z-ADD     80            LENGH            15 5
025000000000     C                   CALL      'QCAEXEC'
025100000000     C                   PARM                    CMD(1)
025200000000     C                   PARM                    LENGH
025300000000     C*
025400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025900000000     C*------------------------------------------------------------------------*
026000960112     C     DECODI        BEGSR
026100000000     C*
026200000000     C* REIMPOSTA INDICATORI DI ERRORE
026300000000     C                   SETOFF                                       606162
026400961021     C                   SETOFF                                       636465
026500110609     C                   SETOFF                                       666768
026600960112     C*--------------------
026700960118     C* ORDINAMENTO
026800960112     C*--------------------
026900960112IF  1C     $ERR          IFEQ      '0'
027000960118IF  2C     V1CORD        IFEQ      '1'
027100960118     C     V1CORD        OREQ      '2'
027200960118     C     V1CORD        OREQ      '3'
027300960118     C     V1CORD        OREQ      '4'
027400960112X   2C                   ELSE
027500960112     C                   SETON                                        60
027600960112     C                   MOVEL     '1'           $ERR
027700960118     C                   MOVEL     MSG(7)        $MSG
027800960112     C                   EXSR      CARMSG
027900960112E   2C                   ENDIF
028000960112E   1C                   ENDIF
028100060323     C*--------------------
028200060323     C* Data dal
028300060323     C*--------------------
028400060323     C                   Z-ADD     *ZEROS        DDAV1C
028500060323IF  1C     $ERR          IFEQ      '0'
028600060323IF  2C     V1CDDA        IFGT      *ZEROS
028700060323     C                   Z-ADD     V1CDDA        WDA8
028800060323     C                   Z-ADD     *ZEROS        DA8W
028900060323     C                   EXSR      CHKDAT
029000060323     C                   Z-ADD     WDA8          V1CDDA
029100060323     C                   Z-ADD     DA8W          DDAV1C
029200060323IF  3C     $ERR          IFEQ      '1'
029300060323     C                   SETON                                        67
029400060323     C                   MOVEL     MSG(8)        $MSG
029500060323     C                   EXSR      CARMSG
029600060323E   3C                   ENDIF
029700060323X   2C                   ELSE
029800060323     C                   SETON                                        67
029900060323     C                   MOVEL     MSG(16)       $MSG
030000060323     C                   EXSR      CARMSG
030100060323E   2C                   ENDIF
030200060323E   1C                   ENDIF
030300960112     C*--------------------
030400060323     C* Data al
030500000000     C*--------------------
030600000000     C                   Z-ADD     *ZEROS        DDEV1C
030700000000IF  1C     $ERR          IFEQ      '0'
030800000000IF  2C     V1CDDE        IFGT      *ZEROS
030900000000     C                   Z-ADD     V1CDDE        WDA8
031000000000     C                   Z-ADD     *ZEROS        DA8W
031100000000     C                   EXSR      CHKDAT
031200000000     C                   Z-ADD     WDA8          V1CDDE
031300000000     C                   Z-ADD     DA8W          DDEV1C
031400000000IF  3C     $ERR          IFEQ      '1'
031500960118     C                   SETON                                        61
031600960118     C                   MOVEL     MSG(8)        $MSG
031700000000     C                   EXSR      CARMSG
031800000000E   3C                   ENDIF
031900040810X   2C                   ELSE
032000040810     C                   SETON                                        61
032100040810     C                   MOVEL     MSG(16)       $MSG
032200040810     C                   EXSR      CARMSG
032300000000E   2C                   ENDIF
032400000000E   1C                   ENDIF
032500060323     C*--------------------
032600060323     C* Data AL < di data Dal
032700060323     C*--------------------
032800060323IF  1C     $ERR          IFEQ      '0'
032900060323     C     DDEV1C        ANDLT     DDAV1C
033000060323     C                   SETON                                        6167
033100060323     C                   MOVEL     '1'           $ERR
033200060323     C                   MOVEL     MSG(18)       $MSG
033300060323     C                   EXSR      CARMSG
033400060323     C                   ENDIF
033500150123     c* massimo un anno
033600150123     c                   movel     ddav1c        annodal           4 0
033700150123     c                   movel     ddev1c        annoal            4 0
033800150123     c                   if        annoal>0
033900150123     c                   eval      diff=annoal-annodal
034000150123     c                   if        diff>1
034100150123     C                   SETON                                        6167
034200150123     C                   MOVEL     '1'           $ERR
034300150123     C                   MOVEL     MSG(22)       $MSG
034400150123     C                   EXSR      CARMSG
034500150123     C                   ENDIF
034600150123     C                   ENDIF
034700960118     C*--------------------
034800960118     C* FILIALE
034900960118     C*--------------------
035000960118     C                   MOVEL     *BLANKS       V1DFIL
035100960118IF  1C     $ERR          IFEQ      '0'
035200960118IF  2C     V1CFIL        IFGT      *ZEROS
035300960118     C                   Z-ADD     V1CFIL        WFIL
035400960118     C                   MOVEL     *BLANKS       WDFIL
035500960118     C                   EXSR      CHKFIL
035600960118     C                   MOVEL     WDFIL         V1DFIL
035700960118IF  3C     $ERR          IFEQ      '1'
035800960118     C                   SETON                                        62
035900960118     C                   MOVEL     MSG(9)        $MSG
036000960118     C                   EXSR      CARMSG
036100960118E   3C                   ENDIF
036200960118E   2C                   ENDIF
036300960118E   1C                   ENDIF
036400960115     C*--------------------
036500960118     C* GIORNO DI SELEZIONE
036600960115     C*--------------------
036700960115IF  1C     $ERR          IFEQ      '0'
036800960115IF  2C     V1CGIO        IFEQ      *BLANKS
036900960115     C     V1CGIO        OREQ      '1'
037000960115     C     V1CGIO        OREQ      '2'
037100960115     C     V1CGIO        OREQ      '3'
037200960115     C     V1CGIO        OREQ      '4'
037300960115     C     V1CGIO        OREQ      '5'
037400960115     C     V1CGIO        OREQ      '6'
037500960115     C     V1CGIO        OREQ      '7'
037600960115X   2C                   ELSE
037700960115     C                   SETON                                        63
037800960115     C                   MOVEL     '1'           $ERR
037900960115     C                   MOVEL     MSG(10)       $MSG
038000960115     C                   EXSR      CARMSG
038100960115E   2C                   ENDIF
038200960115E   1C                   ENDIF
038300961021     C*--------------------
038400961021     C* NUMERO TRAINO DAL-AL
038500961021     C*--------------------
038600961021IF  1C     $ERR          IFEQ      '0'
038700961021IF  2C     V1CTR1        IFGT      *ZEROS
038800961021     C     V1CTR2        ANDGT     *ZEROS
038900961021IF  3C     V1CTR1        IFGT      V1CTR2
039000961021     C                   SETON                                        6465
039100961021     C                   MOVEL     '1'           $ERR
039200961021     C                   MOVEL     MSG(15)       $MSG
039300961021     C                   EXSR      CARMSG
039400961021E   3C                   ENDIF
039500961021E   2C                   ENDIF
039600961021E   1C                   ENDIF
039700060322     C*--------------------
039800060322     C* Trazionista
039900060322     C*--------------------
040000060322IF  1C     $ERR          IFEQ      '0'
040100060322     c                   clear                   v1dpdr
040200060322IF  2C     V1Cpdr        ifne      *blanks
040300060322    1C     V1CPDR        ANDNE     *ZEROS
040400060322     C* RICERCA
040500060322     C     '?'           SCAN      V1CPDR                                 90
040600060322    2C     *IN90         IFEQ      *ON
040700060322     C                   EVAL      PA3TIP = 'T '                                PAR-1
040800060322     C                   clear                   PA3RSC                         PAR-2
040900060322     C                   CLEAR                   PA3CSF                         PAR-3
041000060322     C                   CLEAR                   PA3PDR                         PAR-4
041100060322     C                   CLEAR                   PA3FLG                         PAR-5
041200060322     C                   MOVEL     PARAM3        KPJBU
041300060322     C                   CALL      'TRUL16R'
041400060322     C                   PARM                    KPJBA
041500060322     C                   MOVEL     KPJBU         PARAM3
041600060322     C                   MOVEL     PA3PDR        V1CPDR                         CODICE
041700060322     C                   MOVEL     PA3RSC        V1DPDR                         RAG.SOCIALE
041800060322     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
041900060322    2C                   ENDIF
042000060322     C*
042100060322     C                   TESTN                   V1CPDR               31
042200060322     C                   MOVE      V1CPDR        W001A             1
042300060322    2C     *IN31         IFEQ      *OFF
042400060322     C     W001A         ORLT      '0'
042500060322     C                   MOVEL     '1'           $ERR
042600060322     C                   MOVEL     MSG(17)       $MSG
042700060322     C                   SETON                                        66
042800060322     c                   exsr      carmsg
042900060322   X2C                   ELSE
043000060322     C* CONTROLLO SULL'ANAGRAFICA
043100060322     C                   move      v1cpdr        KTLPDR
043200060322     C     KTNTLZ        CHAIN     TNTLZ01L                           31
043300060322    3C     *IN31         IFEQ      *ON
043400060322     C     TLZATB        ORNE      ' '
043500060322     C                   MOVEL     '1'           $ERR
043600060322     C                   MOVEL     MSG(17)       $MSG
043700060322     C                   SETON                                        66
043800060322     c                   exsr      carmsg
043900060322   X3C                   ELSE
044000060322     C                   MOVEL     TLZRSC        V1DPDR
044100060322    3C                   ENDIF
044200060322    2C                   ENDIF
044300060322E   2C                   ENDIF
044400060322E   1C                   ENDIF
044500110609     C*--------------------
044600110609     C* codice punto di scambio
044700110609     C*--------------------
044800110609     c
044900110609IF  1C                   IF        $ERR ='0'
045000110609     c                   clear                   desloc
045100110609     c                   clear                   desind
045200110609     c                   clear                   descap
045300110609     c                   clear                   desnot
045400110609IF 1aC                   IF        v1cpsc<>*blanks
045500110609     C                   EVAL      INDX=%scan('?':v1cpsc)
045600110609     c
045700110609    2c                   if        indx>0
045800110609     c* imposta la ds di procedura
045900110609     c                   CLEAR                   tntlp0ds
046000110609     c                   MOVE      '01'          dp0op0                         *livello procedura
046100110609     c                   MOVEL     *blanks       dp0op1                         *livello programma
046200110609     c                   MOVEL     '0'           dp0f03                         *premuto F03
046300110609     c                   MOVEL     '0'           dp0f12                         *premuto F12
046400110609     c                   MOVEL     '0'           dp0err                         *errore
046500110609     c                   call      'TNTLP0R'
046600110609     c                   parm                    kpjba
046700110609     c                   parm                    TNTLp0ds
046800110609     c
046900110609     c                   eval      v1cpsc=dp0psc
047000110609     c                   eval      desloc=dp0loc
047100110609     c                   eval      desnot=dp0not
047200110609     c                   if        dp0prv<>*blanks
047300110609     c                   eval      desloc=%trim(dp0loc)+'  '+dp0prv+dp0nar
047400110609     c                   else
047500110609     c                   eval      desloc=%trim(dp0loc)+' '+dp0nar
047600110609     c                   endif
047700110609     c                   eval      desind=dp0ind
047800110609     c                   eval      descap=dp0cap
047900110609     C                   SETON                                        68
048000110609     C                   MOVEL     '1'           $ERR
048100110609     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
048200110609     C                   LEAVEsr
048300110609    2c                   endif
048400110609     c
048500110609     c     v1cpsc        chain     tnpsc01l
048600110609    2c                   if        not %found(tnpsc01l)
048700110609     C                   MOVEL     '1'           $ERR
048800110609     C                   MOVEL     MSG(19)       $MSG
048900110609     C                   EXSR      CARMSG
049000110609     C                   SETON                                        68
049100110609     C                   LEAVEsr
049200110609E   2C                   ENDIF
049300110609     c
049400110609     c                   eval      desloc=pscloc
049500110609     c                   eval      desnot=pscnot
049600110609    2c                   if        pscprv<>*blanks
049700110609     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
049800110609     c                   else
049900110609     c                   eval      desloc=%trim(desloc)+' '+pscnar
050000110609    2c                   endif
050100110609     c                   eval      desind=pscind
050200110609     c                   eval      descap=psccap
050300110609   1ac                   endif
050400110609    1c                   endif
050500110609     c
050600110609     c* Se c'e'  codice di scambiod evo per forza mettere S nella selezione dei traini con scambio
050700110609IF  1C     $ERR          IFEQ      '0'
050800110609    2c                   if        v1cpsc<>*blanks and v1ctpsc<>'S'
050900110609     C                   SETON                                        68
051000110609     C                   MOVEL     '1'           $ERR
051100110609     C                   MOVEL     MSG(21)       $MSG
051200110609     C                   EXSR      CARMSG
051300110609E   2C                   ENDIF
051400110609E   1C                   ENDIF
051500110609
051600960118     C*--------------------
051700960118     C* PRIMO ORDINAMENTO
051800960118     C*--------------------
051900960118IF  1C     $ERR          IFEQ      '0'
052000960118IF  2C     V1CORD        IFEQ      '1'
052100960118IF  3C     V1CFIL        IFGT      *ZEROS
052200960118     C     V1CGIO        ORNE      *BLANKS
052300960118     C                   SETON                                        60
052400960118     C                   MOVEL     '1'           $ERR
052500960118     C                   MOVEL     MSG(11)       $MSG
052600960118     C                   EXSR      CARMSG
052700960118E   2C                   ENDIF
052800960118E   2C                   ENDIF
052900960118E   1C                   ENDIF
053000960118     C*--------------------
053100960118     C* SECONDO ORDINAMENTO
053200960118     C*--------------------
053300960118IF  1C     $ERR          IFEQ      '0'
053400960118IF  2C     V1CORD        IFEQ      '2'
053500960213IF  3C     V1CGIO        IFNE      *BLANKS
053600960118     C                   SETON                                        60
053700960118     C                   MOVEL     '1'           $ERR
053800960118     C                   MOVEL     MSG(12)       $MSG
053900960118     C                   EXSR      CARMSG
054000960213E   3C                   ENDIF
054100960118E   2C                   ENDIF
054200960118E   1C                   ENDIF
054300960118     C*--------------------
054400960118     C* TERZO ORDINAMENTO
054500960118     C*--------------------
054600960118IF  1C     $ERR          IFEQ      '0'
054700960118IF  2C     V1CORD        IFEQ      '3'
054800960118IF  3C     V1CFIL        IFEQ      *ZEROS
054900960118     C                   SETON                                        60
055000960118     C                   MOVEL     '1'           $ERR
055100960118     C                   MOVEL     MSG(13)       $MSG
055200960118     C                   EXSR      CARMSG
055300960213E   3C                   ENDIF
055400960118E   2C                   ENDIF
055500960118E   1C                   ENDIF
055600960118     C*--------------------
055700960118     C* QUARTO ORDINAMENTO
055800960118     C*--------------------
055900960118IF  1C     $ERR          IFEQ      '0'
056000960118IF  2C     V1CORD        IFEQ      '4'
056100960118IF  3C     V1CFIL        IFEQ      *ZEROS
056200960118     C                   SETON                                        60
056300960118     C                   MOVEL     '1'           $ERR
056400960118     C                   MOVEL     MSG(14)       $MSG
056500960118     C                   EXSR      CARMSG
056600960213E   3C                   ENDIF
056700960118E   2C                   ENDIF
056800960118E   1C                   ENDIF
056900000000     C*
057000000000     C                   ENDSR
057100960112     C*------------------------------------------------------------------------*
057200960112     C* CHKFIL - CONTROLLA LA FILIALE
057300960112     C*          INPUT  -> WFIL  - FILIALE
057400960112     C*          OUTPUT -> WDFIL - DESCRIZIONE FILIALE
057500960112     C*------------------------------------------------------------------------*
057600960112     C     CHKFIL        BEGSR
057700960112     C*
057800960116     C     *LIKE         DEFINE    V1CFIL        WFIL
057900960112     C     *LIKE         DEFINE    ORGDES        WDFIL
058000060322     C     *LIKE         DEFINE    TLZPDR        KTLPDR
058100060322     C     *LIKE         DEFINE    TLZTIP        KTLTIP
058200960112     C*
058300960112     C                   MOVEL     *BLANKS       WDFIL
058400960112     C*
058500960112     C                   Z-ADD     WFIL          KORFIL
058600960112     C     KEYORG        CHAIN     AZORG01L                           28
058700960112IF  1C     *IN28         IFEQ      *ON
058800960112     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
058900960112     C                   MOVEL     '1'           $ERR
059000960112X   1C                   ELSE
059100960208     C                   MOVEL     ORGDE8        OG148
059200960208IF  2C     §OGLFV        IFEQ      'N'                                          *IN GESTIONE FV
059300960208     C                   MOVEL     '1'           $ERR
059400960208X   2C                   ELSE
059500960112     C                   MOVEL     ORGDES        WDFIL                          *DESCRIZIONE
059600960208E   2C                   ENDIF
059700960208E   1C                   ENDIF
059800960112     C*
059900960112     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* CHKDAT - CONTROLLA LA DATA
060200000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
060300000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
060400000000     C*------------------------------------------------------------------------*
060500000000     C     CHKDAT        BEGSR
060600000000     C*
060700000000     C     *LIKE         DEFINE    V1CDDE        WDA8
060800000000     C     *LIKE         DEFINE    V1CDDE        DA8W
060900000000     C*
061000000000     C                   Z-ADD     *ZEROS        DA8W
061100000000     C*
061200000000     C                   Z-ADD     WDA8          G08DAT
061300000000     C                   Z-ADD     *ZEROS        G08INV
061400000000     C                   MOVEL     '0'           G08ERR
061500000000     C                   CALL      'XSRDA8'
061600000000     C                   PARM                    WLBDA8
061700000000     C                   Z-ADD     G08DAT        WDA8
061800000000IF  1C     G08ERR        IFEQ      '1'
061900000000     C                   MOVEL     '1'           $ERR
062000000000     C                   ELSE
062100000000     C                   Z-ADD     G08INV        DA8W
062200000000E   1C                   ENDIF
062300000000     C*
062400000000     C                   ENDSR
062500000000     C*------------------------------------------------------------------------*
062600000000     C* IMPVID - IMPOSTAZIONI VIDEO
062700000000     C*------------------------------------------------------------------------*
062800000000     C     IMPVID        BEGSR
062900000000     C*
063000960112     C* CALCOLA LA DATA CORRENTE
063100960112     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
063200960112     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
063300960118     C*
063400960118     C                   Z-ADD     WNUM8         V1CDDE                         *DECORRENZA
063500060323     C                   Z-ADD     WNUM8         V1CDDa                         *DECORRENZA
063600060324     c                   movel     'E'           v1cbis
063700110527     c                   movel     'N'           v1ctpsc
063800960118     C* SE FILIALE
063900020715     c                   If        DutLpo <> 'S'
064000960118     C                   MOVEL     '3'           V1CORD                         *TERZO ORDINAMENTO
064100020715     c                   Z-Add     DutPou        V1cFil
064200960118     C* SE SEDE
064300960118X   1C                   ELSE
064400960118     C                   MOVEL     '1'           V1CORD                         *PRIMO ORDINAMENTO
064500960116E   1C                   ENDIF
064600000000     C*
064700000000     C                   ENDSR
064800000000     C*------------------------------------------------------------------------*
064900000000     C* CARTAB - CARICA LE TABELLE
065000000000     C*------------------------------------------------------------------------*
065100000000     C     CARTAB        BEGSR
065200000000     C*--------------------
065300000000     C* FILIALI
065400000000     C*--------------------
065500000000     C                   Z-ADD     *ZEROS        J                 4 0
065600000000     C     *LOVAL        SETLL     AZORG01L
065700000000     C                   READ      AZORG01L                               99
065800000000DO  1C     *IN99         DOWEQ     *OFF
065900000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
066000960208     C                   MOVEL     ORGDE8        OG148
066100960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
066200000000     C                   ADD       1             J
066300000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
066400000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
066500960208E   3C                   ENDIF
066600960208E   2C                   ENDIF
066700000000     C                   READ      AZORG01L                               99
066800000000E   1C                   ENDDO
066900960112     C*
067000000000     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000000     C* *INZSR - ROUTINE INIZIALE
067300000000     C*------------------------------------------------------------------------*
067400000000     C     *INZSR        BEGSR
067500000000     C*--------------------
067600000000     C* RICEVIMENTO PARAMETRI
067700000000     C*--------------------
067800000000     C     *ENTRY        PLIST
067900000000     C                   PARM                    KPJBA
068000000211     C                   MOVEL     KPJBU         DS00
068100960112     C*--------------------
068200960112     C* VARIABILI RIFERITE AL DATA BASE
068300960112     C*--------------------
068400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
068500000000     C*--------------------
068600000000     C* VARIABILI  N O N  RIFERITE AL DATA BASE
068700000000     C*--------------------
068800000000     C     *LIKE         DEFINE    V1CDDE        DDEV1C
068900060323     C     *LIKE         DEFINE    V1CDDA        DDAV1C
069000000000     C*--------------------
069100000000     C* CHIAVI DI LETTURA
069200000000     C*--------------------
069300000000     C* LETTURA AZORG01L - COMPLETA
069400000000     C     KEYORG        KLIST
069500000000     C                   KFLD                    KORFIL                         *FILIALE
069600060322     C     KTNTLZ        KLIST
069700060322     C                   KFLD                    KTLTIP
069800060322     C                   KFLD                    KTLPDR
069900060322     C                   EVAL      KTLTIP = 'T '
070000000000     C*--------------------
070100000000     C* VARIABILI DI CONTROLLO
070200000000     C*--------------------
070300000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
070400000000     C***
070500000000     C* IMPOSTAZIONI VIDEO
070600000000     C***
070700000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
070800000000     C                   MOVE      D00OP0        INTGES            2
070900000000IF  1C     INTGES        IFEQ      '02'
071000000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
071100000000X   1C                   ELSE
071200000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
071300000000E   1C                   ENDIF
071400000000     C*
071500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
071600000000     C                   EXSR      INZSTD
071700000000     C*
071800000000     C* CARICA LE TABELLE
071900000000     C                   EXSR      CARTAB
072000000000     C*
072100000000     C* IMPOSTAZIONI VIDEO
072200000000     C                   EXSR      IMPVID
072300000000     C*
072400000000     C* EMETTE VIDEATA
072500000000     C                   EXSR      CARV1
072600000000     C*
072700000000     C                   ENDSR
072800000000     C*------------------------------------------------------------------------*
072900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
073200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
073300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
073400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073600000000     C*------------------------------------------------------------------------*
073700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
073800000000     C*------------------------------------------------------------------------*
073900000000     C     INZSTD        BEGSR
074000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
074100000000     C                   CALL      'XKEYS'
074200000000     C                   PARM                    DSKEYS
074300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
074400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
074500000000     C     XMSGPA        PLIST
074600000000     C                   PARM                    DSMSMS
074700000000     C                   PARM                    DSMSPG
074800000000     C                   PARM                    DSMSKE
074900000000     C                   PARM                    DSMSER
075000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
075100000000     C                   MOVEL     'TNTL18R'     DSMSPG                         *NOME PGM CHIAMANTE
075200000000     C                   MOVEL     'TNTL18R'     VMCQUE                         *CODA MESSAGGI DESTI
075300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
075400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
075500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
075600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
075700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
075800000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
075900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
076000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
076100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
076200000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
076300000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
076400000000     C* LANCIO ROUTINE PER VERIFICARE SE
076500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
076600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
076700020715
076800020715     c     *dtaara       define    §azute        azuteds
076900020715     c     *dtaara       define    §datiute      ddatiute
077000020715
077100020715     c                   in(E)     *dtaara
077200020715     c                   if        %Error or rsut = *blanks
077300020715     c                   clear                   tibs34ds
077400020715     c                   call      'TIBS34R'
077500020715     c                   parm                    Tibs34Ds
077600020715     c                   in        *dtaara
077700020715     c                   endif
077800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
077900000000     C                   BITOFF    '01234567'    POSMI1            1
078000000000     C                   BITOFF    '01234567'    POSMI2            1
078100000000     C                   BITOFF    '01234567'    POSMA1            1
078200000000     C                   BITOFF    '01234567'    POSMA2            1
078300000000     C                   BITON     '34'          POSMI1            1            *18
078400000000     C                   BITON     '7'           POSMI2            1            *01
078500000000     C                   BITON     '34'          POSMA1            1            *18
078600000000     C                   BITON     '0'           POSMA2            1            *80
078700000000     C                   MOVE      POSMI2        POSMIN            2
078800000000     C                   MOVEL     POSMI1        POSMIN
078900000000     C                   MOVE      POSMA2        POSMAX            2
079000000000     C                   MOVEL     POSMA1        POSMAX
079100000000     C                   ENDSR
079200000000     C*------------------------------------------------------------------------*
079300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
079400000000     C*------------------------------------------------------------------------*
079500000000     C     FUNSTD        BEGSR
079600000000     C                   SELECT
079700000000     C     KEY           WHENEQ    F3
079800000000     C                   EXSR      F03STD
079900000000     C     KEY           WHENEQ    F12
080000000000     C                   EXSR      F12STD
080100000000     C     KEY           WHENEQ    F24
080200000000     C                   EXSR      F24STD
080300000000     C                   OTHER
080400000000     C                   MOVEL     MSG(2)        $MSG
080500000000     C                   EXSR      CARMSG
080600000000     C                   ENDSL
080700000000     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
081000000000     C*------------------------------------------------------------------------*
081100000000     C     F03STD        BEGSR
081200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
081300000000     C                   MOVEL     '1'           $F03
081400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
081500000000     C                   MOVEL     '1'           $FINE
081600951212     C* PULISCE IL MESSAGGIO
081700951212     C                   MOVEL     *BLANKS       D00MSG
081800000000     C                   ENDSR
081900000000     C*------------------------------------------------------------------------*
082000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
082100000000     C*------------------------------------------------------------------------*
082200000000     C     F12STD        BEGSR
082300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
082400000000     C                   MOVEL     '1'           $F12
082500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
082600000000     C                   MOVEL     '1'           $FINE
082700951212     C* PULISCE IL MESSAGGIO
082800951212     C                   MOVEL     *BLANKS       D00MSG
082900000000     C                   ENDSR
083000000000     C*------------------------------------------------------------------------*
083100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
083200000000     C*------------------------------------------------------------------------*
083300000000     C     F24STD        BEGSR
083400000000     C*
083500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
083600000000     C                   EXSR      OUTFUN
083700000000     C*
083800000000     C                   ENDSR
083900000000     C*------------------------------------------------------------------------*
084000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
084100000000     C*          CODA MESSAGGI DEL PGM
084200000000     C*------------------------------------------------------------------------*
084300000000     C     CARMSG        BEGSR
084400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
084500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
084600000000     C     MSGULT        IFEQ      0
084700000000     C                   MOVEL     '1'           *IN40
084800000000     C                   WRITE     TL18DVK
084900000000     C                   MOVEL     '0'           *IN40
085000000000     C                   ENDIF
085100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
085200000000     C                   MOVEL     $MSG          DSMSMS
085300000000     C                   CALL      'XMSG'        XMSGPA
085400000000     C     DSMSER        IFEQ      0
085500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
085600000000     C                   ENDIF
085700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
085800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
085900000000     C                   WRITE     TL18DVM                                      *SFL MESS
086000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
086100000000     C                   MOVEL     *BLANKS       $MSG
086200000000     C                   ENDSR
086300000000     C*------------------------------------------------------------------------*
086400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
086500000000     C*------------------------------------------------------------------------*
086600000000     C     OUTMSG        BEGSR
086700000000     C* VISUALIZZAZIONE MESSAGGIO
086800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
086900000000     C                   WRITE     TL18DVK                                      *CONTR MESS
087000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
087100000000     C                   Z-ADD     *ZEROS        MSGULT
087200000000     C                   ENDSR
087300000000     C*------------------------------------------------------------------------*
087400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
087500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
087600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
087700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
087800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
087900000000     C*------------------------------------------------------------------------*
088000000000     C     CARFUN        BEGSR
088100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
088200000000     C     1             DO        31            I                 5 0          --- 1 -->
088300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
088400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
088500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
088600020715     c                   If        DutLpo = 'S'
088700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
088800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
088900000000     C                   ELSE                                                   *INTERROG.
089000000000     C                   MOVEL     FUNASI        FAT(I)
089100000000     C                   ENDIF
089200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
089300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
089400000000     C                   MOVEL     FUNAFG        FAT(I)
089500000000     C                   ELSE                                                   *INTERROG.
089600000000     C                   MOVEL     FUNAFI        FAT(I)
089700000000     C                   ENDIF
089800000000     C                   ENDIF                                                  <-- 2 ---
089900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
090000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
090100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
090200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
090300000000     C                   ELSE
090400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
090500000000     C                   ENDIF
090600000000     C                   ENDIF
090700000000     C                   ENDDO                                                  <-- 1 ---
090800000000     C                   ENDSR
090900000000     C*------------------------------------------------------------------------*
091000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
091100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
091200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
091300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
091400000000     C*------------------------------------------------------------------------*
091500000000     C     OUTFUN        BEGSR
091600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
091700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
091800000000     C                   ENDDO
091900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
092000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
092100000000     C     1             DO        31            I
092200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
092300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
092400000000     C                   ADD       1             FUNNAT
092500000000     C                   ENDIF
092600000000     C                   ENDDO
092700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
092800000000     C     FUNNAT        IFGT      8
092900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
093000000000     C                   ENDIF
093100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
093200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
093300000000     C                   Z-ADD     8             FUNVIS            5 0
093400000000     C     FAT(23)       IFEQ      'S'
093500000000     C                   SUB       1             FUNVIS
093600000000     C                   ENDIF
093700000000     C     FAT(24)       IFEQ      'S'
093800000000     C                   SUB       1             FUNVIS
093900000000     C                   ENDIF
094000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
094100000000     C                   Z-ADD     *ZEROS        W                 5 0
094200000000     C                   ADD       1             FUNULT
094300000000     C     FUNULT        IFGT      31
094400000000     C                   Z-ADD     1             FUNULT
094500000000     C                   ENDIF
094600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
094700000000     C     FUNULT        DO        31            I                              --- 1 -->
094800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
094900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
095000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
095100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
095200000000     C     FDE(I)        ANDNE     *BLANKS
095300000000     C     I             IFEQ      23                                           *TOLGO F23
095400000000     C     I             OREQ      24                                           *E F24
095500000000     C                   ELSE
095600000000     C                   ADD       1             W
095700000000     C                   MOVEL     FDE(I)        FUV(W)
095800000000     C                   ENDIF
095900000000     C                   ENDIF                                                  <-- 3 ---
096000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
096100000000     C                   Z-ADD     I             FUNULT
096200000000     C                   ENDIF                                                  <-- 2 ---
096300000000     C                   ENDDO                                                  <-- 1 ---
096400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
096500000000     C                   MOVEL     FUV(1)        VFD001
096600000000     C                   MOVEL     FUV(2)        VFD002
096700000000     C                   MOVEL     FUV(3)        VFD003
096800000000     C                   MOVEL     FUV(4)        VFD004
096900000000     C                   MOVEL     FUV(5)        VFD005
097000000000     C                   MOVEL     FUV(6)        VFD006
097100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
097200000000     C     FAT(24)       ANDEQ     'N'
097300000000     C                   MOVEL     FUV(7)        VFD007
097400000000     C                   MOVEL     FUV(8)        VFD008
097500000000     C                   ELSE                                                   <-- 1 -->
097600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
097700000000     C     FAT(24)       ANDEQ     'S'
097800000000     C                   MOVEL     FDE(23)       VFD007
097900000000     C                   MOVEL     FDE(24)       VFD008
098000000000     C                   ELSE                                                   <-- 2 -->
098100000000     C                   MOVEL     FUV(7)        VFD007
098200000000     C     FAT(23)       IFEQ      'S'
098300000000     C                   MOVEL     FDE(23)       VFD008
098400000000     C                   ELSE
098500000000     C                   MOVEL     FDE(24)       VFD008
098600000000     C                   ENDIF
098700000000     C                   ENDIF                                                  <-- 2 ---
098800000000     C                   ENDIF                                                  <-- 1 ---
098900000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
099000000000     C                   ENDSR
099100000000     C*------------------------------------------------------------------------*
099200000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
099300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
099400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
099500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
099600000000     C*------------------------------------------------------------------------*
099700000000     C     CONFUN        BEGSR
099800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
099900000000     C     1             DO        31            I                              --- 1 -->
100000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
100100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
100200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
100300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
100400000000     C                   EXSR      CARMSG
100500000000     C                   MOVEL     '1'           $ERR
100600000000     C                   ENDIF                                                  <-- 3 ---
100700000000     C                   ENDIF                                                  <-- 2 ---
100800000000     C                   ENDDO                                                  <-- 1 ---
100900000000     C                   ENDSR
101000000000     O*------------------------------------------------------------------------*
101100000000** MSG - (MESSAGGI)
101200000000TNTL18R- Tasto funzionale non valido.                                          1
101300000000TNTL18R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
101400000000TNTL18R- Opzione non valida.                                                   3
101500000000TNTL18R- Opzione abilitata ma non gestita. Avvertire CED.                      4
101600000000TNTL18R- Inizio elenco.                                                        5
101700000000TNTL18R- Fine elenco.                                                          6
101800960118TNTL18R- Richiedere un ordinamento fra quelli possibili                        7
101900960118TNTL18R- Data errata                                                           8
102000961022TNTL18R- Punto operativo errato                                                9
102100960118TNTL18R- Giorno di selezione errato                                           10
102200961022TNTL18R- Richiedere solo la decorrenza per il 1° ordinamento                  11
102300961022TNTL18R- Richiedere solo decorrenza e/o punto operativo per il 2° ordinamento 12
102400961022TNTL18R- Immettere il punto operativo per il 3° ordinamento                   13
102500961022TNTL18R- Immettere il punto operativo per il 4° ordinamento                   14
102600961021TNTL18R- Il numero traino dal deve essere minore del numero traino al         15
102700040810TNTL18R- Data obbligatoria.                                                   16
102800060322TNTL18R- Codice TRAZIONISTA obbligatorio o inesistente in anagrafica
102900060323TNTL18R- "Data Al" minore di "Data Dal"
103000110609TNTL18R- codice punti di scambio errato
103100960112TNTL18R- Stampa lanciata ....                                                 20
103200150123Se parzializza.per punti di scambio immettere "S=Si" in traino con scambio    21
103300150123E' possibile riichiedere massimo un anno di traini in decorrenza              22
103400000000** CMD  (COMANDI)
103500000000WRKSPLF
103600000000
103700000000
103800000000
103900000000
104000000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
104100000000F1    -                   -    NN     NN
104200000000F2    -                   -    NN     NN
104300951213F3    -F3=Fine            -    SS     SS
104400000000F4    -                   -    NN     NN
104500000000F5    -                   -    NN     NN
104600000000F6    -F6=Conferma        -    SS     SS
104700000000F7    -                   -    NN     NN
104800000000F8    -                   -    NN     NN
104900000000F9    -                   -    NN     NN
105000000000F10   -                   -    NN     NN
105100000000F11   -                   -    NN     NN
105200951206F12   -F12=Ritorno        -    SS     SS
105300000000F13   -                   -    NN     NN
105400000000F14   -                   -    NN     NN
105500000000F15   -                   -    NN     NN
105600000000F16   -                   -    NN     NN
105700000000F17   -                   -    NN     NN
105800000000F18   -                   -    NN     NN
105900000000F19   -                   -    NN     NN
106000000000F20   -                   -    NN     NN
106100000000F21   -                   -    NN     NN
106200000000F22   -F22=Gestione stampe-    EE     EE
106300000000F23   -                   -    NN     NN
106400000000F24   -F24=Altri tasti    -    NN     NN
106500000000ENTER -                   -    SS     SS
106600000000ROLLUP-                   -    NN     NN
106700000000ROLLDN-                   -    NN     NN
106800000000HELP  -                   -    NN     NN
106900000000PRINT -                   -    NN     NN
107000000000BSPACE-                   -    NN     NN
107100000000CLEAR -                   -    NN     NN

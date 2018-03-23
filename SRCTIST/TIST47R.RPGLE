000100111129     /*END
000200000000     H*------------------------------------------------------------------------*
000300060829     H* CONTROLLO ASSORBIMENTI X LNA
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900060830     FTIST47D   CF   E             WORKSTN INFDS(DEVDS1)
001000060830     F                                     SFILE(ST47DMS:MSGULT)
001100060925     F                                     SFILE(ST47S02:wS02NRR)
001200060831     FAZORG01L  IF   E           K DISK
001300060831     FTABEL00F  IF   E           K DISK
001400060830     FTNTBE01L  IF   E           K DISK
001500060830     FTIALA00F  IF   E             DISK    USROPN
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* MESSAGGI
002100000000     D*-------------------
002200000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* DESCRIZIONI TASTI FUNZIONALI
002500000000     D*-------------------
002600000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700000000     D*-------------------
002800000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002900000000     D*-------------------
003000000000     D FUV             S             19    DIM(24)
003100060830     D*-------------------
003200060830     D* SCHIERA SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
003300060830     D*-------------------
003400060830     D SPVKG_DA        S                   DIM(200) LIKE(§ALAPESDA) INZ
003500060830     D SPVKG_A         S                   DIM(200) LIKE(§ALAPESA)  INZ
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900000000     D*-------------------
004000000000     D* RIDEFINIZIONE SCHIERA -FUD-
004100000000     D*-------------------
004200000000     D FUDDS           DS
004300000000     D*                -DESCRIZIONE PARZIALE
004400000000     D  FUDDE1                 1      1
004500000000     D*                -FUNZIONE
004600000000     D  FUDFUN                 2      3
004700000000     D*                -DESCRIZIONE PARZIALE
004800000000     D  FUDDE2                 4     19
004900000000     D*                -FILLER
005000000000     D  FUDFI0                20     34
005100000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005200000000     D  FUDATT                35     35
005300000000     D*                -FILLER
005400000000     D  FUDFI1                36     80
005500000000     D*                -DESCRIZIONE COMPLETA
005600000000     D  FUDDES                 1     19
005700000000     D*                -RIDEFINIZIONE RIGA
005800000000     D  FUDRIG                 1     80
005900000000     D*-------------------
006000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006100000000     D*-------------------
006200060831     D DSMSG         E DS
006300000000     D*-------------------
006400000000     D* DS GESTIONE TASTI FUNZIONALI
006500000000     D*-------------------
006600000000     D DSKEY         E DS
006700000000     D DEVDS1          DS
006800000000     D  KEY                  369    369
006900000000     D*-------------------
007000000000     D* DS "XSRDA8" - CONTROLLO DATA(8)
007100000000     D*-------------------
007200971128     D WLBDA8          DS
007300000000     D  G08DAT                 1      8  0
007400000000     D  G08INV                 9     16  0
007500000000     D  G08ERR                17     17
007600000000     D  G08TGI                18     22  0
007700060830     D*-------------------
007800060830     D* DS "XSRLAV8" - CALCOLO GIORNI LAVORATIVI TRA 2 DATE
007900060830     D*-------------------
008000060830     D WL08            DS
008100060830     D  L08DA                  1      8  0
008200060830     D  L08A                   9     16  0
008300060830     D  L08L                  17     21  0
008400060830     D*-------------------
008500060830     D* DS SCAGLIONI PESO / VOLUME
008600060830     D*-------------------
008700060830     D DALA          E DS
008800061016     D*-------------------
008900061016     D* DS CODICE AREA
009000061016     D*-------------------
009100061016     D DS05          E DS
009200060830     D*-------------------
009300000000     D* ARCHITETTURA
009400000000     D*-------------------
009500000000     D KPJBA         E DS
009600001016     D*-------------------
009700001016     D* DS PARAMETRI DI LANCIO PGM
009800001016     D*-------------------
009900060829     D PARAM           DS                  INZ
010000060829     D  PAROPE                        1
010100060829     D  PARDAI                        8  0
010200060829     D  PARDAF                        8  0
010300060901     D  PARIAD                        8  0
010400060901     D  PARFAD                        8  0
010500060830     D  PARGIO                        3  0
010600060830     D  PARDET                        1
010700061027     D  PARORD                        1
010800060830     D  PARKG1DA                      7  1
010900060830     D  PARKG1A                       7  1
011000060830     D  PARKG2DA                      7  1
011100060830     D  PARKG2A                       7  1
011200060830     D  PARKG3DA                      7  1
011300060830     D  PARKG3A                       7  1
011400060830     D  PARKG4DA                      7  1
011500060830     D  PARKG4A                       7  1
011600060830     D  PARDIS                        1
011700060830     D  PARARE                        3  0
011800060830     D  PARFIL                        3  0
011900060830     D  PARCAD                        2
012000060830     D  PARCAA                        2
012100060830     D*-------------------
012200060830     D* VARIABILI D WRK
012300060830     D*-------------------
012400060925     D wS02NRR         S              4  0
012500111129
012600111129       //--------------------------------------------------------------
012700111129       //?Definizione aree dati.                                       ?
012800111129       //--------------------------------------------------------------
012900111129
013000111129       // -?Dati utente?
013100111129     d §AzUte        e ds                  extname(AZUTE00F)
013200111129     d                                     dtaara
013300111129     d §DatiUte      e ds                  extname(dDatiUte)
013400111129     d                                     dtaara
013500111129
013600111129       //--------------------------------------------------------------
013700111129       //?Definizione prototipi procedure.                             ?
013800111129       //--------------------------------------------------------------
013900111129
014000111129       // -?Reperimento dati utente?
014100111129     d TIBS34ds      e ds                  inz
014200111129      /copy gaitrasrc/srcProtoPR,TIBS34R
014300111129
014400000000     C*------------------------------------------------------------------------*
014500000000     C* MAIN LINE
014600000000     C*------------------------------------------------------------------------*
014700060830     C*
014800060830     C* Caricamento scaglioni di peso / volume
014900060830     C                   exsr      carspv
015000010417     C*
015100010417     C* POI INIZIALIZZO LA DS DI PASSAGGIO PARAMETRI
015200010417     C                   RESET                   PARAM
015300000000     C*
015400000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
015500000000IF  1C     ERRL00        IFEQ      '0'
015600000000     C*
015700000000     C* CICLO FINO A CHE:
015800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
015900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
016000000000DO  2C     F03L00        DOUEQ     '1'
016100000000     C     F12L00        OREQ      '1'
016200000000     C*
016300000000     C* EMETTE FORMATO VIDEO
016400000000     C                   EXSR      BUTFMR
016500000000     C*
016600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
016700000000     C                   EXSR      FUNCON
016800000000     C*
016900000000E   2C                   ENDDO
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   SETON                                        LR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
017500000000     C*------------------------------------------------------------------------*
017600000000     C     BUTFMR        BEGSR
017700000000     C*
017800000000     C* EMETTE TESTATA
017900060830     C                   WRITE     ST47D01
018000000000     C*
018100000000     C* VISUALIZZA MESSAGGIO
018200000000     C                   EXSR      SNDMSG
018300000000     C*
018400000000     C* EMETTE FUNZIONI
018500060830     C                   WRITE     ST47D02
018600000000     C*
018700000000     C* EMETTE FORMATO VIDEO
018800060830     C                   EXFMT     ST47D03
018900000000     C*
019000000000     C* AZZERA FLAG DI ERRORE LIVELLO
019100000000     C                   MOVEL     '0'           ERRL00
019200000000     C*
019300000000     C* AZZERA INDICATORI DI ERRORE
019400060830     C                   SETOFF                                       707172
019500060830     C                   SETOFF                                       808182
019600060830     C                   SETOFF                                       838485
019700060830     C                   SETOFF                                       8687
019800060830     C                   SETOFF                                       909192
019900060831     C                   SETOFF                                       9394
020000000000     C*
020100000000     C                   ENDSR
020200000000     C*------------------------------------------------------------------------*
020300000000     C* SNDMSG - MANDA MESSAGGIO
020400000000     C*------------------------------------------------------------------------*
020500000000     C     SNDMSG        BEGSR
020600000000     C*
020700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
020800000000     C                   MOVEL     '1'           *IN40
020900060830     C                   WRITE     ST47DMC
021000000000     C                   MOVEL     '0'           *IN40
021100000000     C*
021200000000     C* VISUALIZZAZIONE MESSAGGIO
021300000000     C                   Z-ADD     0             MSGULT            4 0
021400000000     C                   CALL      'XMSG'        XMSGPA
021500000000     C     DSMSER        IFEQ      0
021600000000     C                   MOVEL     DSMSKE        MSGKEY
021700000000     C                   ENDIF
021800000000     C                   Z-ADD     1             MSGULT
021900060830     C                   WRITE     ST47DMS                                      SFL MESS
022000060830     C                   WRITE     ST47DMC                                      CONTR MESS
022100000000     C*
022200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
022300000000     C                   MOVEL     *BLANKS       DSMSMS
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
022800000000     C*------------------------------------------------------------------------*
022900000000     C     ENTGES        BEGSR
023000000000     C*
023100000000     C* CONTROLLA I CAMPI IMMESSI
023200000000     C                   EXSR      DECODI
023300000000     C*
023400000000     C                   ENDSR
023500001106     C*------------------------------------------------------------------------*
023600060829     C* F01GES - GESTIONE TASTO FUNZIONALE F01=AIUTO
023700001106     C*------------------------------------------------------------------------*
023800001106     C     F01GES        BEGSR
023900060830     C*
024000060830     C                   movel     '0'           wrksfl02          1
024100060830
024200060830      /free
024300060830
024400060830       // pulisco il subfile
024500060830        exsr sr_pulsfl;
024600060830
024700060830       // carico il subfile con i dati che trovo o vuoto
024800060830        exsr sr_carsfl;
024900060925
025000060925        S02NRR  = 1;
025100060925        wS02NRR = 1;
025200060830
025300060830        wrksfl02 = *on;
025400060830
025500060830        dou wrksfl02 = *off;
025600060830         write st47p02;
025700060830         exfmt st47c02;
025800060830
025900060830       // F12=Ritorno
026000060830          if *inkl;
026100060830           wrksfl02 = *off;
026200060830           leavesr;
026300060830          endif;
026400060830
026500060830         enddo;
026600060830
026700060830      /end-free
026800001106     C*
026900001106     C                   ENDSR
027000060830
027100000000     C*------------------------------------------------------------------------*
027200000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
027300000000     C*------------------------------------------------------------------------*
027400000000     C     F03GES        BEGSR
027500000000     C*
027600000000     C* IMPOSTA FLAG F03
027700000000     C                   MOVEL     '1'           F03L00
027800000000     C*
027900000000     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
028200000000     C*------------------------------------------------------------------------*
028300000000     C     F06GES        BEGSR
028400000000     C*
028500000000     C* CONTROLLA I CAMPI IMMESSI
028600000000     C                   EXSR      DECODI
028700000000     C*
028800000000     C* LANCIA IL PROGRAMMA DI STAMPA
028900000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
029000000000     C*
029100000000     C* CARICA I TFP SELEZIONATI
029200000000     C                   EXSR      VIDPAR
029300010417     C                   MOVEL(p)  PARAM         KPJBU
029400060829     C                   MOVEL     'ST48'        KCOAZ
029500111130     c                   if        knmus = *all'1'
029600111130     c                   call      'TIST48C'
029700111130     c                   parm                    kpjba
029800111130     c                   else
029900070118     C                   CALL      'BCH10'
030000000000     C                   PARM                    KPJBA
030100111130     c                   endif
030200060829     C*
030300060829     C* QUINDI ESCO DAL PROGRAMMA
030400060829     C                   MOVEL     '1'           F03L00
030500000000E   1C                   ENDIF
030600000000     C*
030700000000     C                   ENDSR
030800000000     C*------------------------------------------------------------------------*
030900000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
031000000000     C*------------------------------------------------------------------------*
031100000000     C     F12GES        BEGSR
031200000000     C*
031300000000     C* IMPOSTO FLAG F12
031400000000     C                   MOVEL     '1'           F12L00
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
031900000000     C*------------------------------------------------------------------------*
032000000000     C     F24GES        BEGSR
032100000000     C*
032200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
032300000000     C                   EXSR      FUNGES
032400000000     C*
032500000000     C                   ENDSR
032600000000     C*------------------------------------------------------------------------*
032700001016     C* DECODI - CONTROLLA CAMPI E DECODIFICA
032800000000     C*------------------------------------------------------------------------*
032900000000     C     DECODI        BEGSR
033000060830     C*
033100060830     C* AZZERA INDICATORI DI ERRORE
033200060830     C                   SETOFF                                       707172
033300060830     C                   SETOFF                                       808182
033400060830     C                   SETOFF                                       838485
033500060830     C                   SETOFF                                       8687
033600060830     C                   SETOFF                                       909192
033700060831     C                   SETOFF                                       9394
033800060830     C*--------------------
033900060830     C* DATA INIZIALE
034000060830     C*--------------------
034100060830     C     ERRL00        IFEQ      '0'
034200060830     C                   Z-ADD     VIDDTI        G08DAT
034300060830     C                   Z-ADD     *ZEROS        G08INV
034400060830     C                   MOVEL     '0'           G08ERR
034500060830     C                   CALL      'XSRDA8'
034600060830     C                   PARM                    WLBDA8
034700060830     C                   Z-ADD     G08DAT        VIDDTI
034800060830     C     G08ERR        IFEQ      '0'
034900060830     C                   Z-ADD     G08INV        WDTI              8 0
035000060830     C                   ELSE
035100060830     C                   MOVEL     '1'           ERRL00
035200060830     C                   MOVEL     MSG(16)       DSMSMS
035300060830     C                   SETON                                        70
035400060830     C                   ENDIF
035500060830     C                   ENDIF
035600060830     C*--------------------
035700060830     C* DATA FINALE
035800060830     C*--------------------
035900060830     C     ERRL00        IFEQ      '0'
036000060830     C                   Z-ADD     VIDDTF        G08DAT
036100060830     C                   Z-ADD     *ZEROS        G08INV
036200060830     C                   MOVEL     '0'           G08ERR
036300060830     C                   CALL      'XSRDA8'
036400060830     C                   PARM                    WLBDA8
036500060830     C                   Z-ADD     G08DAT        VIDDTF
036600060830     C     G08ERR        IFEQ      '0'
036700060830     C                   Z-ADD     G08INV        WDTF              8 0
036800060830     C                   ELSE
036900060830     C                   MOVEL     '1'           ERRL00
037000060830     C                   MOVEL     MSG(16)       DSMSMS
037100060830     C                   SETON                                        71
037200060830     C                   ENDIF
037300060830     C                   ENDIF
037400060830     C*--------------------
037500060830     C* CONGRUENZA LIMITI FASCE DI PESO
037600060830     C*--------------------
037700060830     C     ERRL00        IFEQ      '0'
037800060830     C                   IF        VIDKG1DA > *zeros OR VIDKG1A > *zeros
037900060830     C                   IF        VIDKG1DA >= VIDKG1A
038000060830     C                   MOVEL     '1'           ERRL00
038100060830     C                   MOVEL     MSG(9)        DSMSMS
038200060830     C                   SETON                                        8081
038300060830     C                   ENDIF
038400060830     C                   ENDIF
038500060830     C                   ENDIF
038600060830     C*
038700060830     C     ERRL00        IFEQ      '0'
038800060830     C                   IF        VIDKG2DA > *zeros OR VIDKG2A > *zeros
038900060830     C                   IF        VIDKG2DA >= VIDKG2A
039000060830     C                   MOVEL     '1'           ERRL00
039100060830     C                   MOVEL     MSG(9)        DSMSMS
039200060830     C                   SETON                                        8283
039300060830     C                   ENDIF
039400060830     C                   ENDIF
039500060830     C                   ENDIF
039600060830     C*
039700060830     C     ERRL00        IFEQ      '0'
039800060830     C                   IF        VIDKG3DA > *zeros OR VIDKG3A > *zeros
039900060830     C                   IF        VIDKG3DA >= VIDKG3A
040000060830     C                   MOVEL     '1'           ERRL00
040100060830     C                   MOVEL     MSG(9)        DSMSMS
040200060830     C                   SETON                                        8485
040300060830     C                   ENDIF
040400060830     C                   ENDIF
040500060830     C                   ENDIF
040600060830     C*
040700060830     C     ERRL00        IFEQ      '0'
040800060830     C                   IF        VIDKG4DA > *zeros OR VIDKG4A > *zeros
040900060830     C                   IF        VIDKG4DA >= VIDKG4A
041000060830     C                   MOVEL     '1'           ERRL00
041100060830     C                   MOVEL     MSG(9)        DSMSMS
041200060830     C                   SETON                                        8687
041300060830     C                   ENDIF
041400060830     C                   ENDIF
041500060830     C                   ENDIF
041600060830     C*--------------------
041700060830     C* OBBLIGATORIETA ALMENO 1 FASCIA DI PESO
041800060830     C*--------------------
041900060830     C     ERRL00        IFEQ      '0'
042000060830     C                   IF        VIDKG1A = *zeros AND
042100060830     C                             VIDKG2A = *zeros AND
042200060830     C                             VIDKG3A = *zeros AND
042300060830     C                             VIDKG4A = *zeros
042400060830     C                   MOVEL     '1'           ERRL00
042500060830     C                   MOVEL     MSG(10)       DSMSMS
042600060830     C                   SETON                                        808284
042700060830     C                   SETON                                        86
042800060830     C                   SETON                                        818385
042900060830     C                   SETON                                        87
043000060830     C                   ENDIF
043100060830     C                   ENDIF
043200060830     C*--------------------
043300060830     C* CONTROLLO LIMITI FASCE DI PESO
043400060830     C*--------------------
043500060830     C     ERRL00        IFEQ      '0'
043600060830     C                   IF        VIDKG1DA > *zeros
043700060830     C     VIDKG1DA      LOOKUP    SPVKG_DA                               55
043800060830     C                   IF        not %found
043900060830     C                   MOVEL     '1'           ERRL00
044000060830     C                   MOVEL     MSG(6)        DSMSMS
044100060830     C                   SETON                                        80
044200060830     C                   ENDIF
044300060830     C                   ENDIF
044400060830     C                   ENDIF
044500060830     C     ERRL00        IFEQ      '0'
044600060830     C                   IF        VIDKG1A > *zeros
044700060830     C     VIDKG1A       LOOKUP    SPVKG_A                                55
044800060830     C                   IF        not %found
044900060830     C                   MOVEL     '1'           ERRL00
045000060830     C                   MOVEL     MSG(7)        DSMSMS
045100060830     C                   SETON                                        81
045200060830     C                   ENDIF
045300060830     C                   ENDIF
045400060830     C                   ENDIF
045500060830     C*
045600060830     C     ERRL00        IFEQ      '0'
045700060830     C                   IF        VIDKG2DA > *zeros
045800060830     C     VIDKG2DA      LOOKUP    SPVKG_DA                               55
045900060830     C                   IF        not %found
046000060830     C                   MOVEL     '1'           ERRL00
046100060830     C                   MOVEL     MSG(6)        DSMSMS
046200060830     C                   SETON                                        82
046300060830     C                   ENDIF
046400060830     C                   ENDIF
046500060830     C                   ENDIF
046600060830     C     ERRL00        IFEQ      '0'
046700060830     C                   IF        VIDKG2A > *zeros
046800060830     C     VIDKG2A       LOOKUP    SPVKG_A                                55
046900060830     C                   IF        not %found
047000060830     C                   MOVEL     '1'           ERRL00
047100060830     C                   MOVEL     MSG(7)        DSMSMS
047200060830     C                   SETON                                        83
047300060830     C                   ENDIF
047400060830     C                   ENDIF
047500060830     C                   ENDIF
047600060830     C*
047700060830     C     ERRL00        IFEQ      '0'
047800060830     C                   IF        VIDKG3DA > *zeros
047900060830     C     VIDKG3DA      LOOKUP    SPVKG_DA                               55
048000060830     C                   IF        not %found
048100060830     C                   MOVEL     '1'           ERRL00
048200060830     C                   MOVEL     MSG(6)        DSMSMS
048300060830     C                   SETON                                        84
048400060830     C                   ENDIF
048500060830     C                   ENDIF
048600060830     C                   ENDIF
048700060830     C     ERRL00        IFEQ      '0'
048800060830     C                   IF        VIDKG3A > *zeros
048900060830     C     VIDKG3A       LOOKUP    SPVKG_A                                55
049000060830     C                   IF        not %found
049100060830     C                   MOVEL     '1'           ERRL00
049200060830     C                   MOVEL     MSG(7)        DSMSMS
049300060830     C                   SETON                                        85
049400060830     C                   ENDIF
049500060830     C                   ENDIF
049600060830     C                   ENDIF
049700060830     C*
049800060830     C     ERRL00        IFEQ      '0'
049900060830     C                   IF        VIDKG4DA > *zeros
050000060830     C     VIDKG4DA      LOOKUP    SPVKG_DA                               55
050100060830     C                   IF        not %found
050200060830     C                   MOVEL     '1'           ERRL00
050300060830     C                   MOVEL     MSG(6)        DSMSMS
050400060830     C                   SETON                                        86
050500060830     C                   ENDIF
050600060830     C                   ENDIF
050700060830     C                   ENDIF
050800060830     C     ERRL00        IFEQ      '0'
050900060830     C                   IF        VIDKG4A > *zeros
051000060830     C     VIDKG4A       LOOKUP    SPVKG_A                                55
051100060830     C                   IF        not %found
051200060830     C                   MOVEL     '1'           ERRL00
051300060830     C                   MOVEL     MSG(7)        DSMSMS
051400060830     C                   SETON                                        87
051500060830     C                   ENDIF
051600060830     C                   ENDIF
051700060830     C                   ENDIF
051800060831     C*--------------------
051900060831     C* SCELTA/CONTROLLO DISTRETTO
052000060831     C*--------------------
052100060831     C     ERRL00        IFEQ      '0'
052200060831     C                   IF        VIDDIS <> *blanks
052300060831     C                   MOVEL(P)  VIDDIS        WDIV              1
052400060831     C                   EXSR      CHKDIV
052500060831     C                   IF        ERRL00 = '1'
052600060831     C                   MOVEL     MSG(11)       DSMSMS
052700060831     C                   SETON                                        90
052800060831     C                   ENDIF
052900060831     C                   MOVEL(P)  WDIV          VIDDIS
053000060831     C                   ENDIF
053100060831     C                   ENDIF
053200060831     C*--------------------
053300060831     C* SCELTA/CONTROLLO AREA
053400060831     C*--------------------
053500060831     C     ERRL00        IFEQ      '0'
053600060831     C                   IF        VIDARE <> *blanks
053700060831     C                   MOVEL(P)  VIDARE        WARE              3
053800060831     C                   EXSR      CHKARE
053900060831     C                   IF        ERRL00 = '1'
054000060831     C                   MOVEL     MSG(12)       DSMSMS
054100060831     C                   SETON                                        91
054200060831     C                   ENDIF
054300060831     C                   MOVEL(P)  WARE          VIDARE
054400111130     c                   if        ErrL00 = *off
054500111130     c     §05PCP        chain     AZORG
054600111130     c                   if        Not %found(AZORG01L)  or
054700111130     c                             (VIDdis <> *blank  and
054800111130     c                              VIDdis <> ORGfl3)
054900111130     c                   eval      ErrL00 = *on
055000111130     c                   eval      DSMSMS = Msg(17)
055100111130     c                   eval      *in90 = *on
055200111130     c                   endif
055300111130     c                   endif
055400060831     C                   ENDIF
055500060831     C                   ENDIF
055600060831     C*--------------------
055700111130     C* SCELTA/CONTROLLO FIL.
055800060831     C*--------------------
055900060831     C     ERRL00        IFEQ      '0'
056000060831     C                   IF        VIDFIL <> *blanks
056100060831     C                   MOVEL(P)  VIDFIL        WFIL              3
056200060831     C                   EXSR      CHKFIL
056300060831     C                   IF        ERRL00 = '1'
056400060831     C                   MOVEL     MSG(13)       DSMSMS
056500060831     C                   SETON                                        92
056600060831     C                   ENDIF
056700060831     C                   MOVEL(P)  WFIL          VIDFIL
056800111130     c                   if        ErrL00 =  *off     and
056900111130     c                             VIDdis <> *blanks  and
057000111130     c                             VIDdis <> ORGfl3
057100111130     c                   eval      ErrL00 = *on
057200111130     c                   eval      DSMSMS = Msg(18)
057300111130     c                   eval      *in90  = *on
057400111130     c                   endif
057500111130     c                   if        ErrL00 =  *off    and
057600111130     c                             VIDare >  *zeros  and
057700111130     c                             VIDare <> %editc( ORGcar : 'X' )
057800111130     c                   eval      ErrL00 = *on
057900111130     c                   eval      DSMSMS = Msg(19)
058000111130     c                   eval      *in91  = *on
058100111130     c                   endif
058200060831     C                   ENDIF
058300060831     C                   ENDIF
058400060831     C*--------------------
058500060831     C* CONTROLLO CAP "DA - A"
058600060831     C*--------------------
058700060831     C     ERRL00        IFEQ      '0'
058800060831     C                   IF        VIDCAD = *blanks
058900060831     C                   EVAL      VIDCAD = '00'
059000060831     C                   ENDIF
059100060831     C*
059200060831     C                   IF        VIDCAA = *blanks
059300060831     C                   EVAL      VIDCAA = '99'
059400060831     C                   ENDIF
059500060831     C                   ENDIF
059600060831     C*
059700060831     C     ERRL00        IFEQ      '0'
059800060831     C                   IF        VIDCAD > VIDCAA
059900060831     C                   MOVEL     '1'           ERRL00
060000060831     C                   MOVEL     MSG(15)       DSMSMS
060100060831     C                   SETON                                        9394
060200060831     C                   ENDIF
060300060831     C                   ENDIF
060400060831     C*--------------------
060500060904     C* CONTROLLO "NO TUTTO" (SOLO SE NO UTENTE EDP*)
060600060831     C*--------------------
060700060831     C     ERRL00        IFEQ      '0'
060800060904     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
060900060831     C                   IF        VIDDIS = *blanks AND
061000060831     C                             VIDARE = *blanks AND
061100060831     C                             VIDFIL = *blanks AND
061200060831     C                             (VIDCAD = *blanks OR VIDCAD = '00') AND
061300060831     C                             (VIDCAA = *blanks OR VIDCAA = '99')
061400060831     C                   MOVEL     '1'           ERRL00
061500060831     C                   MOVEL     MSG(14)       DSMSMS
061600060831     C                   SETON                                        909192
061700060831     C                   SETON                                        9394
061800060904     C                   ENDIF
061900060831     C                   ENDIF
062000060831     C                   ENDIF
062100060830     C*
062200000000     C                   ENDSR
062300000000     C*------------------------------------------------------------------------*
062400000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
062500000000     C*------------------------------------------------------------------------*
062600000000     C     AZZVID        BEGSR
062700010417     C*
062800010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
062900060830     C                   CLEAR                   ST47D03
063000060830     C*
063100060830     C* CARICO LE DATE INIZIALE E FINALE DAL FILE
063200060830     C                   OPEN      tiala00f
063300060830     C                   READ      tiala00f
063400060830     C                   CLOSE     tiala00f
063500060830     C*
063600060830     C* Giro la data iniziale
063700060830     C                   Z-ADD     *zeros        G08DAT
063800060830     C                   Z-ADD     alaDAI        G08INV
063900060830     C                   MOVEL     '3'           G08ERR
064000060830     C                   CALL      'XSRDA8'
064100060830     C                   PARM                    WLBDA8
064200060830     C                   Z-ADD     G08DAT        VIDDTI
064300060830     C*
064400060830     C* Giro la data finale
064500060830     C                   Z-ADD     *zeros        G08DAT
064600060830     C                   Z-ADD     alaDAF        G08INV
064700060830     C                   MOVEL     '3'           G08ERR
064800060830     C                   CALL      'XSRDA8'
064900060830     C                   PARM                    WLBDA8
065000060830     C                   Z-ADD     G08DAT        VIDDTF
065100060830     C*
065200060830     C* GIORNI LAVORATIVI
065300060830     C                   CLEAR                   WL08
065400060830     C                   Z-ADD     alaDAI        L08DA
065500060830     C                   Z-ADD     alaDAF        L08A
065600060830     C                   CALL      'XSRLAV8'
065700060830     C                   PARM                    WL08
065800060830     C                   MOVE      L08L          VIDGIO
065900060830     C*
066000060830     C* IMPOSTO GLI ALTRI VALORI D DEFAULT
066100060830     C                   EVAL      VIDDET = 'S'
066200061027     C                   EVAL      VIDORD = 'C'
066300060830     C                   EVAL      VIDCAD = '00'
066400060830     C                   EVAL      VIDCAA = '99'
066500060831     C                   EVAL      VIDDISD = '(" "   = tutte)'
066600060831     C                   EVAL      VIDARED = '("   " = tutte)'
066700060831     C                   EVAL      VIDFILD = '("   " = tutti)'
066800000000     C*
066900000000     C                   ENDSR
067000000000     C*------------------------------------------------------------------------*
067100001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
067200000000     C*------------------------------------------------------------------------*
067300000000     C     VIDPAR        BEGSR
067400000000     C*
067500060830     C                   MOVEL     'S'           PAROPE                         * F = genera File
067600001016     C                   Z-ADD     WDTI          PARDAI
067700001016     C                   Z-ADD     WDTF          PARDAF
067800060901     C                   Z-ADD     VIDDTI        PARIAD
067900060901     C                   Z-ADD     VIDDTF        PARFAD
068000060830     C                   Z-ADD     VIDGIO        PARGIO
068100060830     C                   MOVEL     VIDDET        PARDET
068200061027     C                   MOVEL     VIDORD        PARORD
068300060830     C                   Z-ADD     VIDKG1DA      PARKG1DA
068400060830     C                   Z-ADD     VIDKG1A       PARKG1A
068500060830     C                   Z-ADD     VIDKG2DA      PARKG2DA
068600060830     C                   Z-ADD     VIDKG2A       PARKG2A
068700060830     C                   Z-ADD     VIDKG3DA      PARKG3DA
068800060830     C                   Z-ADD     VIDKG3A       PARKG3A
068900060830     C                   Z-ADD     VIDKG4DA      PARKG4DA
069000060830     C                   Z-ADD     VIDKG4A       PARKG4A
069100060830     C                   MOVEL     VIDDIS        PARDIS
069200060831     C                   MOVEL     VIDARE        PARARE
069300060831     C                   MOVEL     VIDFIL        PARFIL
069400060830     C                   MOVEL     VIDCAD        PARCAD
069500060830     C                   MOVEL     VIDCAA        PARCAA
069600001016     C*
069700000000     C                   ENDSR
069800060830       // ------------------------------------------------------------------------
069900060830       // pulisco il subfile delle descrizioni in lingua
070000060830       // ------------------------------------------------------------------------
070100060830
070200060830      /free
070300060830
070400060830       begsr sr_pulsfl;
070500060830
070600060925        clear ws02nrr;
070700060830        *in24 = *off;
070800060830        *in25 = *off;
070900060830        write st47c02;
071000060830        *in24 = *on;
071100060830        *in25 = *on;
071200060830
071300060830       endsr;
071400060830
071500060830       // ------------------------------------------------------------------------
071600060830       // carico il subfile delle descrizioni in lingua
071700060830       // ------------------------------------------------------------------------
071800060830       begsr sr_carsfl;
071900060830
072000060830        *in30 = *off;
072100060830       // carico la tabella "ALA"
072200060830         clear dala;
072300060830         tbecod = 'ALA';
072400060830         setll tbecod tntbe01l;
072500060830         if %found(tntbe01l);
072600060831         reade (tbecod) tntbe01l;
072700060830         dow not %eof(tntbe01l);
072800060830          if tbeatb = *blanks;
072900060830           dala = tbeuni;
073000060830           vidw02sgl = tbeke1;
073100060830           vidw02des = §aladesc;
073200060925           ws02nrr = ws02nrr + 1;
073300060830           write st47s02;
073400060830          endif;
073500060831          reade (tbecod) tntbe01l;
073600060830        enddo;
073700060830        endif;
073800060830
073900060830        *in30 = *on;
074000060830
074100060830       endsr;
074200060830
074300060830      /end-free
074400060830
074500060830     C*------------------------------------------------------------------------*
074600060830     C* CARSPV - CARICA GLI SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
074700060830     C*------------------------------------------------------------------------*
074800060830     C     CARSPV        BEGSR
074900060830     C*
075000060830     C* Inizializzo le schiere d memorizzazione
075100060830     C                   clear                   SPVKG_DA
075200060830     C                   clear                   SPVKG_A
075300060830     C*
075400060830     C                   z-add     0             k                 3 0
075500060830     C                   movel     'ALA'         tbeCOD
075600060830     C     tbeCOD        setll     tntbe01l
075700060830     C                   if        %equal(tntbe01l)
075800060830     C     tbeCOD        reade     tntbe01l
075900060830     C                   dow       not %eof(tntbe01l)
076000060830     C                   if        tbeATb = *blanks
076100060830     C                   add       1             k
076200060830     C                   eval      DALA = tbeUNI
076300060830     C                   eval      SPVKG_DA(k) = §alapesda
076400060830     C                   eval      SPVKG_A(k)  = §alapesa
076500060830     C                   endif
076600060830     C     tbeCOD        reade     tntbe01l
076700060830     C                   enddo
076800060830     C                   endif
076900060830     C*
077000060830     C                   ENDSR
077100060831     C*------------------------------------------------------------------------*
077200060831     C* CHKDIV - CONTROLLA LA DIVISIONE
077300060831     C*------------------------------------------------------------------------*
077400060831     C     CHKDIV        BEGSR
077500060831     C*
077600060831     C* RICHIESTA INTERROGAZIONE ELENCO
077700060831     C     '?'           SCAN      WDIV                                   99
077800060831     C     *IN99         IFEQ      *ON
077900060831     C                   Z-ADD     1             §KUT
078000060831     C                   MOVEL     '17'          §COD
078100060831     C                   MOVEL     *BLANKS       §KEY
078200060831     C                   MOVEL     *BLANKS       §DES
078300060831     C                   CALL      'X§TABER'
078400060831     C                   PARM                    §KUT              1 0
078500060831     C                   PARM                    §COD              2
078600060831     C                   PARM                    §KEY              8
078700060831     C                   PARM                    §DES             30
078800060831     C                   MOVEL     §KEY          WDIV
078900060831     C                   ENDIF
079000060831     C*
079100060831     C* CONTROLLA ESISTENZA CODICE
079200060831     C                   Z-ADD     1             tblKUT
079300060831     C                   MOVEL     '17'          tblCOD
079400060831     C                   MOVEL(P)  WDIV          tblKEY
079500060831     C     KEYtab_c      CHAIN     TABEL00F                           99
079600060831     C     *IN99         IFEQ      *ON
079700060831     C     tblFLG        OREQ      '*'
079800060831     C                   MOVEL     '1'           ERRL00
079900060831     C                   ELSE
080000060831     C                   MOVEL     tblUNI        VIDDISD
080100060831     C                   ENDIF
080200060831     C*
080300060831     C                   ENDSR
080400060831     C*------------------------------------------------------------------------*
080500060831     C* CHKARE - CONTROLLA L'AREA
080600060831     C*------------------------------------------------------------------------*
080700060831     C     CHKARE        BEGSR
080800060831     C*
080900060831     C* RICHIESTA INTERROGAZIONE ELENCO
081000060831     C     '?'           SCAN      WARE                                   99
081100060831     C     *IN99         IFEQ      *ON
081200060831     C                   Z-ADD     1             §KUT
081300060831     C                   MOVEL     '05'          §COD
081400060831     C                   MOVEL     *BLANKS       §KEY
081500060831     C                   MOVEL     *BLANKS       §DES
081600060831     C                   CALL      'X§TABER'
081700060831     C                   PARM                    §KUT              1 0
081800060831     C                   PARM                    §COD              2
081900060831     C                   PARM                    §KEY              8
082000060831     C                   PARM                    §DES             30
082100060831     C                   MOVEL     §KEY          WARE
082200060831     C                   ENDIF
082300060831     C*
082400060831     C* CARATTERI ALFABETICI NON VALIDI
082500060831     C     WARE          IFLT      *ZEROS
082600060831     C                   MOVEL     '1'           ERRL00
082700060831     C                   ELSE
082800060831     C*
082900060831     C* CONTROLLA ESISTENZA CODICE
083000060831     C                   Z-ADD     1             tblKUT
083100060831     C                   MOVEL     '05'          tblCOD
083200060831     C                   MOVEL(P)  WARE          tblKEY
083300060831     C     KEYtab_c      CHAIN     TABEL00F                           99
083400060831     C     *IN99         IFEQ      *ON
083500060831     C     tblFLG        OREQ      '*'
083600060831     C                   MOVEL     '1'           ERRL00
083700060831     C                   ELSE
083800061016     C                   MOVEL     tblUNI        DS05
083900061016     C                   MOVEL(P)  §05DES        VIDARED
084000111129     C*//  §05PCP        CHAIN     AZORG01L
084100111129     C*//                IF        %found(AZORG01L)
084200111129     C*//                EVAL      VIDDIS = ORGFL3
084300111129     C*//                ENDIF
084400060831     C                   ENDIF
084500060831     C                   ENDIF
084600060831     C*
084700060831     C                   ENDSR
084800060831     C*------------------------------------------------------------------------*
084900060831     C* CHKFIL - CONTROLLA LA FILIALE
085000060831     C*------------------------------------------------------------------------*
085100060831     C     CHKFIL        BEGSR
085200060831     C*
085300060831     C* RICHIESTA INTERROGAZIONE ELENCO
085400060831     C     '?'           SCAN      WFIL                                   99
085500060831     C     *IN99         IFEQ      *ON
085600060831     C                   MOVEL     *BLANKS       §COD1
085700060831     C                   MOVE      *BLANKS       §DES1
085800060831     C                   MOVE      *BLANKS       §PRA
085900060831     C                   CALL      'TNSD24R'
086000060831     C                   PARM                    §COD1             3
086100060831     C                   PARM                    §TIP              1
086200060831     C                   PARM                    §DES1            25
086300060831     C                   PARM                    §PRA              3
086400060831     C                   MOVEL     §COD1         WFIL
086500060831     C                   ENDIF
086600060831     C*
086700060831     C* CARATTERI ALFABETICI NON VALIDI
086800060831     C     WFIL          IFLT      *ZEROS
086900060831     C                   MOVEL     '1'           ERRL00
087000060831     C                   ELSE
087100060831     C*
087200060831     C* CONTROLLA ESISTENZA CODICE
087300060831     C                   MOVEL     WFIL          orgFIL
087400060831     C     orgFIL        CHAIN     AZORG01L                           99
087500060831     C     *IN99         IFEQ      *ON
087600060831     C     orgFAG        OREQ      'V'
087700060831     C     orgFVA        ORNE      *BLANKS
087800060831     C                   MOVEL     '1'           ERRL00
087900060831     C                   ELSE
088000060831     C                   MOVEL     ORGDES        VIDFILD
088100111129     C*//                EVAL      VIDDIS = ORGFL3
088200111129     C*//                MOVE(P)   ORGCAR        VIDARE
088300061016     C                   MOVE(P)   ORGFIL        VIDFIL
088400060831     C                   ENDIF
088500060831     C                   ENDIF
088600060831     C*
088700060831     C                   ENDSR
088800000000     C*------------------------------------------------------------------------*
088900000000     C* *INZSR - OOPERAZIONI INIZIALI
089000000000     C*------------------------------------------------------------------------*
089100000000     C     *INZSR        BEGSR
089200000000     C*--------------------
089300000000     C* RICEVIMENTO PARAMETRI
089400000000     C*--------------------
089500000000     C     *ENTRY        PLIST
089600000000     C                   PARM                    KPJBA
089700060831     C*--------------------
089800060831     C* CHIAVI DI LETTURA
089900060831     C*--------------------
090000060831     C*
090100060831     C* CHIAVE LETTURA TABEL00F - COMPLETA
090200060831     C     KEYtab_c      KLIST
090300060831     C                   KFLD                    tblKUT
090400060831     C                   KFLD                    tblCOD
090500060831     C                   KFLD                    tblKEY
090600000000     C*--------------------
090700060829     C* VARIABILI NN RIFERITE AL DATABASE
090800000000     C*--------------------
090900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
091000000000     C*
091100000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
091200000000     C     XMSGPA        PLIST
091300000000     C                   PARM                    DSMSMS
091400000000     C                   PARM                    DSMSPG
091500000000     C                   PARM                    DSMSKE
091600000000     C                   PARM                    DSMSER
091700000000     C*
091800000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
091900000000     C                   CALL      'XKEY'
092000000000     C                   PARM                    DSKEY
092100000000     C*
092200000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
092300000000     C*   -NOME PGM CHIAMANTE
092400000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
092500060830     C                   MOVEL     'TIST47R'     DSMSPG
092600060830     C                   MOVEL     'TIST47R'     MSGQUE
092700000000     C                   MOVEL     *BLANKS       MSGKEY
092800000000     C                   MOVEL     *BLANKS       DSMSMS
092900111129      *
093000111129      * - Reperimento dati job
093100111129     c                   exsr      sr_DatiJob
093200000000     C*
093300000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
093400000000     C                   Z-ADD     0             FUNGRU            5 0
093500000000     C*
093600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
093700000000     C                   EXSR      FUNGES
093800000000     C*--------------------
093900000000     C* VARIABILI DI CONTROLLO
094000000000     C*--------------------
094100000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
094200000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
094300000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
094400000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
094500000000     C*
094600000000     C* PULISCE IL VIDEO DA EMETTERE
094700000000     C                   EXSR      AZZVID
094800000000     C*
094900000000     C                   ENDSR
095000111129      /free
095100111129       //--------------------------------------------------------------
095200111129       //?Reperimento Dati del job (Utente/Operativi).                 ?
095300111129       //--------------------------------------------------------------
095400111129       BEGSR  sr_DatiJob;
095500111129
095600111129         in(e) §AzUte;
095700111129         if NOT %error;
095800111129           in(e) §DatiUte;
095900111129         endif;
096000111129         if %error or RSut = *blank;
096100111129           tibs34r ( tibs34ds );
096200111129           in §AzUte;
096300111129           in §DatiUte;
096400111129         endif;
096500111129
096600111129       ENDSR;
096700111129      /end-free
096800000000     C*------------------------------------------------------------------------*
096900000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
097000000000     C*------------------------------------------------------------------------*
097100000000     C     FUNGES        BEGSR
097200000000     C*
097300000000     C* PULISCO CAMPI DI LAVORO
097400000000     C                   Z-ADD     0             W                 5 0
097500000000     C*
097600000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
097700000000     C* E CARICO TABELLA FUNZIONI ATTIVE
097800000000     C     1             DO        24            I
097900000000     C                   MOVEL     FUD(I)        FUDRIG
098000000000     C*
098100000000     C* PULIZIA FUNZIONI PRECEDENTI
098200000000     C                   MOVEL     *BLANKS       FUV(I)
098300000000     C*
098400000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
098500000000     C* (SALTA F24)
098600000000     C     FUDATT        IFEQ      'S'
098700000000     C     FUDFUN        ANDNE     '24'
098800000000     C                   ADD       1             W
098900000000     C                   MOVEL     FUDDES        FUV(W)
099000000000     C                   END
099100000000     C                   END                                                    DO WHILE
099200000000     C*
099300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
099400000000     C* ALTRIMENTI DISATTIVO F24
099500000000     C     1             DO        24            I
099600000000     C                   MOVEL     FUD(I)        FUDRIG
099700000000     C*
099800000000     C     FUDFUN        IFEQ      '24'
099900000000     C     W             IFGT      6
100000000000     C                   MOVEL     'S'           FUDATT
100100000000     C                   ELSE
100200000000     C                   MOVEL     *BLANKS       FUDATT
100300000000     C                   END                                                    W GT 8
100400000000     C                   MOVEL     FUDRIG        FUD(I)
100500000000     C                   END
100600000000     C                   END                                                    DO WHILE
100700000000     C*
100800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
100900000000     C     W             DIV       6             I
101000000000     C                   MVR                     Z
101100000000     C*
101200000000     C     Z             IFNE      0
101300000000     C     I             ADD       1             Z
101400000000     C                   ELSE
101500000000     C                   Z-ADD     I             Z                 5 0
101600000000     C                   END
101700000000     C*
101800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
101900000000     C                   ADD       1             FUNGRU
102000000000     C     FUNGRU        IFGT      Z
102100000000     C     FUNGRU        ORLT      1
102200000000     C                   Z-ADD     1             FUNGRU
102300000000     C                   END
102400000000     C*
102500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
102600000000     C     FUNGRU        IFEQ      1
102700000000     C                   MOVEL     FUV(1)        FUN001
102800000000     C                   MOVEL     FUV(2)        FUN002
102900000000     C                   MOVEL     FUV(3)        FUN003
103000000000     C                   MOVEL     FUV(4)        FUN004
103100000000     C                   MOVEL     FUV(5)        FUN005
103200000000     C                   MOVEL     FUV(6)        FUN006
103300000000     C                   END
103400000000     C*
103500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
103600000000     C     FUNGRU        IFEQ      2
103700000000     C                   MOVEL     FUV(7)        FUN001
103800000000     C                   MOVEL     FUV(8)        FUN002
103900000000     C                   MOVEL     FUV(9)        FUN003
104000000000     C                   MOVEL     FUV(10)       FUN004
104100000000     C                   MOVEL     FUV(11)       FUN005
104200000000     C                   MOVEL     FUV(12)       FUN006
104300000000     C                   END
104400000000     C*
104500000000     C* TERZO GRUPPO DI 6 FUNZIONI
104600000000     C     FUNGRU        IFEQ      3
104700000000     C                   MOVEL     FUV(13)       FUN001
104800000000     C                   MOVEL     FUV(14)       FUN002
104900000000     C                   MOVEL     FUV(15)       FUN003
105000000000     C                   MOVEL     FUV(16)       FUN004
105100000000     C                   MOVEL     FUV(17)       FUN005
105200000000     C                   MOVEL     FUV(18)       FUN006
105300000000     C                   END
105400000000     C*
105500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
105600000000     C     FUNGRU        IFEQ      4
105700000000     C                   MOVEL     FUV(19)       FUN001
105800000000     C                   MOVEL     FUV(20)       FUN002
105900000000     C                   MOVEL     FUV(21)       FUN003
106000000000     C                   MOVEL     FUV(22)       FUN004
106100000000     C                   MOVEL     *BLANKS       FUN005
106200000000     C                   MOVEL     *BLANKS       FUN006
106300000000     C                   END
106400000000     C*
106500000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
106600000000     C                   MOVEL     *BLANKS       FUN007
106700000000     C                   MOVEL     *BLANKS       FUN008
106800000000     C*
106900000000     C     1             DO        24            I
107000000000     C                   MOVEL     FUD(I)        FUDRIG
107100000000     C     FUDFUN        IFEQ      '24'
107200000000     C     FUDATT        ANDEQ     'S'
107300000000     C                   MOVEL     FUDDES        FUN008
107400000000     C                   END
107500000000     C                   END                                                    DO
107600000000     C*
107700000000     C                   ENDSR
107800000000     C*------------------------------------------------------------------------*
107900000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
108000000000     C*------------------------------------------------------------------------*
108100000000     C     FUNCON        BEGSR
108200000000     C*
108300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
108400000000     C                   EXSR      FUNVAR
108500000000     C*
108600000000     C* PULISCE CAMPI DI LAVORO
108700000000     C                   Z-ADD     0             I                 5 0
108800000000     C                   Z-ADD     0             W                 5 0
108900000000     C*
109000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
109100000000IF  1C     FUNKEY        IFNE      *BLANKS
109200000000DO  2C     1             DO        24            I
109300000000     C                   MOVEL     FUD(I)        FUDRIG
109400000000     C*
109500000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
109600000000IF  3C     FUNKEY        IFEQ      FUDFUN
109700000000     C     FUDATT        ANDEQ     'S'
109800000000     C                   Z-ADD     I             W
109900000000E   3C                   ENDIF
110000000000E   2C                   ENDDO
110100000000     C*
110200000000     C* FUNZIONE NON ATTIVA, ERRORE
110300000000IF  2C     W             IFEQ      0
110400000000     C                   MOVEL     MSG(1)        DSMSMS
110500000000E   2C                   ENDIF
110600000000E   1C                   ENDIF
110700000000     C*
110800000000     C* FUNZIONE ATTIVA, PROCEDE
110900000000IF  1C     W             IFNE      0
111000000000     C     FUNKEY        OREQ      *BLANKS
111100000000IF  2C     FUNKEY        IFEQ      *BLANKS
111200000000     C                   EXSR      ENTGES
111300000000E   2C                   ENDIF
111400060829IF  2C     FUNKEY        IFEQ      '01'
111500060829     C                   EXSR      F01GES                                       *FINE LAVORO
111600060829E   2C                   ENDIF
111700000000IF  2C     FUNKEY        IFEQ      '03'
111800000000     C                   EXSR      F03GES                                       *FINE LAVORO
111900000000E   2C                   ENDIF
112000000000IF  2C     FUNKEY        IFEQ      '06'
112100000000     C                   EXSR      F06GES                                       *LANCIO
112200000000E   2C                   ENDIF
112300000000IF  2C     FUNKEY        IFEQ      '12'
112400000000     C                   EXSR      F12GES                                       *RITORNO
112500000000E   2C                   ENDIF
112600000000IF  2C     FUNKEY        IFEQ      '24'
112700000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
112800000000E   2C                   ENDIF
112900000000E   1C                   ENDIF
113000000000     C*
113100000000     C                   ENDSR
113200000000     C*------------------------------------------------------------------------*
113300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
113400000000     C*------------------------------------------------------------------------*
113500000000     C     FUNVAR        BEGSR
113600000000     C*
113700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
113800000000     C*
113900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
114000000000     C     KEY           IFEQ      ENTER
114100000000     C                   MOVEL     *BLANKS       FUNKEY
114200000000     C                   END
114300000000     C     KEY           IFEQ      F1
114400000000     C                   MOVEL     '01'          FUNKEY
114500000000     C                   END
114600000000     C     KEY           IFEQ      F2
114700000000     C                   MOVEL     '02'          FUNKEY
114800000000     C                   END
114900000000     C     KEY           IFEQ      F3
115000000000     C                   MOVEL     '03'          FUNKEY
115100000000     C                   END
115200000000     C     KEY           IFEQ      F4
115300000000     C                   MOVEL     '04'          FUNKEY
115400000000     C                   END
115500000000     C     KEY           IFEQ      F5
115600000000     C                   MOVEL     '05'          FUNKEY
115700000000     C                   END
115800000000     C     KEY           IFEQ      F6
115900000000     C                   MOVEL     '06'          FUNKEY
116000000000     C                   END
116100000000     C     KEY           IFEQ      F7
116200000000     C                   MOVEL     '07'          FUNKEY
116300000000     C                   END
116400000000     C     KEY           IFEQ      F8
116500000000     C                   MOVEL     '08'          FUNKEY
116600000000     C                   END
116700000000     C     KEY           IFEQ      F9
116800000000     C                   MOVEL     '09'          FUNKEY
116900000000     C                   END
117000000000     C     KEY           IFEQ      F10
117100000000     C                   MOVEL     '10'          FUNKEY
117200000000     C                   END
117300000000     C     KEY           IFEQ      F11
117400000000     C                   MOVEL     '11'          FUNKEY
117500000000     C                   END
117600000000     C     KEY           IFEQ      F12
117700000000     C                   MOVEL     '12'          FUNKEY
117800000000     C                   END
117900000000     C     KEY           IFEQ      F13
118000000000     C                   MOVEL     '13'          FUNKEY
118100000000     C                   END
118200000000     C     KEY           IFEQ      F14
118300000000     C                   MOVEL     '14'          FUNKEY
118400000000     C                   END
118500000000     C     KEY           IFEQ      F15
118600000000     C                   MOVEL     '15'          FUNKEY
118700000000     C                   END
118800000000     C     KEY           IFEQ      F16
118900000000     C                   MOVEL     '16'          FUNKEY
119000000000     C                   END
119100000000     C     KEY           IFEQ      F17
119200000000     C                   MOVEL     '17'          FUNKEY
119300000000     C                   END
119400000000     C     KEY           IFEQ      F18
119500000000     C                   MOVEL     '18'          FUNKEY
119600000000     C                   END
119700000000     C     KEY           IFEQ      F19
119800000000     C                   MOVEL     '19'          FUNKEY
119900000000     C                   END
120000000000     C     KEY           IFEQ      F20
120100000000     C                   MOVEL     '20'          FUNKEY
120200000000     C                   END
120300000000     C     KEY           IFEQ      F21
120400000000     C                   MOVEL     '21'          FUNKEY
120500000000     C                   END
120600000000     C     KEY           IFEQ      F22
120700000000     C                   MOVEL     '22'          FUNKEY
120800000000     C                   END
120900000000     C     KEY           IFEQ      F23
121000000000     C                   MOVEL     '23'          FUNKEY
121100000000     C                   END
121200000000     C     KEY           IFEQ      F24
121300000000     C                   MOVEL     '24'          FUNKEY
121400000000     C                   END
121500000000     C     KEY           IFEQ      ROLLUP
121600000000     C                   MOVEL     'RU'          FUNKEY
121700000000     C                   END
121800000000     C     KEY           IFEQ      HELP
121900000000     C                   MOVEL     'HP'          FUNKEY
122000000000     C                   END
122100000000     C                   ENDSR
122200000000     C*------------------------------------------------------------------------*
122300000000     C* NOTE PER COMPILAZIONE SCHIERE
122400000000     C*------------------------------------------------------------------------*
122500000000     C* -MSG-
122600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
122700000000     C* (NON TOCCARE MSG,1)
122800000000     C*
122900000000     C* -CMD-
123000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
123100000000     C*
123200000000     C* -FUD-
123300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
123400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
123500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
123600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
123700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
123800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
123900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
124000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
124100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
124200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
124300000000     C*
124400000000     C*------------------------------------------------------------------------*
124500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
124600060830TIST47R- Tasto funzionale non gestito                                          1
124700111130TIST47R- Distretto non valido                                                  2 - Libero
124800111130TIST47R- Area non valida                                                       3 - Libero
124900111130TIST47R- Filiale non valida                                                    4 - Libero
125000111130TIST47R- Limite fascia errato (F01 per consultazione scaglioni)                5 - Libero
125100060830TIST47R- Limite "DA" non valido (F01 per consultazione scaglioni)              6
125200060830TIST47R- Limite "A" non valido (F01 per consultazione scaglioni)               7
125300111130TIST47R- Inserire i giorni lavorativi                                          8 - Libero
125400060831TIST47R- Limite "DA" maggiore limite "A"                                       9
125500060830TIST47R- Inserire almeno 1 fascia di peso                                     10
125600060831TIST47R- Distretto non valido                                                 11
125700060831TIST47R- Area non valida                                                      12
125800111130TIST47R- Filiale non valida                                                   13
125900060831TIST47R- Effettuare almeno una selezione                                      14
126000060831TIST47R- CAP "DAL" maggiore CAP "AL"                                          15
126100060830TIST47R- Data non valida                                                      16
126200111130TIST47R- Distretto non corrispondente a quello dell'area inserita             17
126300111130TIST47R- Distretto non corrispondente a quello della filiale inserita         18
126400111130TIST47R- Area non corrispondente a quella della filiale inserita              19
126500060830TIST47R-                                                                      20
126600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
126700060829F01=Aiuto          -        (S/N)=S  <== S-FISSO           funz.01¬
126800000000                   -        (S/N)=N                        funz.02¬
126900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
127000000000                   -        (S/N)=N                        funz.04¬
127100000000                   -        (S/N)=N                        funz.05¬
127200060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
127300000000                   -        (S/N)=N                        funz.07¬
127400000000                   -        (S/N)=N                        funz.08¬
127500000000                   -        (S/N)=N                        funz.09¬
127600060829                   -        (S/N)=N                        funz.10¬
127700000000                   -        (S/N)=N                        funz.11¬
127800060830F12=Ritorno        -        (S/N)=N                        funz.12¬
127900000000                   -        (S/N)=N                        funz.13¬
128000000000                   -        (S/N)=N                        funz.14¬
128100000000                   -        (S/N)=N                        funz.15¬
128200000000                   -        (S/N)=N                        funz.16¬
128300000000                   -        (S/N)=N                        funz.17¬
128400000000                   -        (S/N)=N                        funz.18¬
128500000000                   -        (S/N)=N                        funz.19¬
128600000000                   -        (S/N)=N                        funz.20¬
128700000000                   -        (S/N)=N                        funz.21¬
128800060829                   -        (S/N)=N                        funz.22¬
128900000000                   -        (S/N)=N                        funz.23¬
129000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

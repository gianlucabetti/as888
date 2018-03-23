000100000000     H*------------------------------------------------------------------------*
000200060829     H* CONTROLLO ASSORBIMENTI X LNA
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800060830     FTIST47D   CF   E             WORKSTN INFDS(DEVDS1)
000900060830     F                                     SFILE(ST47DMS:MSGULT)
001000060925     F                                     SFILE(ST47S02:wS02NRR)
001100060831     FAZORG01L  IF   E           K DISK
001200060831     FTABEL00F  IF   E           K DISK
001300060830     FTNTBE01L  IF   E           K DISK
001400060830     FTIALA00F  IF   E             DISK    USROPN
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* MESSAGGI
002000000000     D*-------------------
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200000000     D*-------------------
002300000000     D* DESCRIZIONI TASTI FUNZIONALI
002400000000     D*-------------------
002500000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600000000     D*-------------------
002700000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002800000000     D*-------------------
002900000000     D FUV             S             19    DIM(24)
003000060830     D*-------------------
003100060830     D* SCHIERA SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
003200060830     D*-------------------
003300060830     D SPVKG_DA        S                   DIM(200) LIKE(§ALAPESDA) INZ
003400060830     D SPVKG_A         S                   DIM(200) LIKE(§ALAPESA)  INZ
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800000000     D*-------------------
003900000000     D* RIDEFINIZIONE SCHIERA -FUD-
004000000000     D*-------------------
004100000000     D FUDDS           DS
004200000000     D*                -DESCRIZIONE PARZIALE
004300000000     D  FUDDE1                 1      1
004400000000     D*                -FUNZIONE
004500000000     D  FUDFUN                 2      3
004600000000     D*                -DESCRIZIONE PARZIALE
004700000000     D  FUDDE2                 4     19
004800000000     D*                -FILLER
004900000000     D  FUDFI0                20     34
005000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005100000000     D  FUDATT                35     35
005200000000     D*                -FILLER
005300000000     D  FUDFI1                36     80
005400000000     D*                -DESCRIZIONE COMPLETA
005500000000     D  FUDDES                 1     19
005600000000     D*                -RIDEFINIZIONE RIGA
005700000000     D  FUDRIG                 1     80
005800000000     D*-------------------
005900000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006000000000     D*-------------------
006100060831     D DSMSG         E DS
006200000000     D*-------------------
006300000000     D* DS GESTIONE TASTI FUNZIONALI
006400000000     D*-------------------
006500000000     D DSKEY         E DS
006600000000     D DEVDS1          DS
006700000000     D  KEY                  369    369
006800000000     D*-------------------
006900000000     D* DS "XSRDA8" - CONTROLLO DATA(8)
007000000000     D*-------------------
007100971128     D WLBDA8          DS
007200000000     D  G08DAT                 1      8  0
007300000000     D  G08INV                 9     16  0
007400000000     D  G08ERR                17     17
007500000000     D  G08TGI                18     22  0
007600060830     D*-------------------
007700060830     D* DS "XSRLAV8" - CALCOLO GIORNI LAVORATIVI TRA 2 DATE
007800060830     D*-------------------
007900060830     D WL08            DS
008000060830     D  L08DA                  1      8  0
008100060830     D  L08A                   9     16  0
008200060830     D  L08L                  17     21  0
008300060830     D*-------------------
008400060830     D* DS SCAGLIONI PESO / VOLUME
008500060830     D*-------------------
008600060830     D DALA          E DS
008700061016     D*-------------------
008800061016     D* DS CODICE AREA
008900061016     D*-------------------
009000061016     D DS05          E DS
009100060830     D*-------------------
009200000000     D* ARCHITETTURA
009300000000     D*-------------------
009400000000     D KPJBA         E DS
009500001016     D*-------------------
009600001016     D* DS PARAMETRI DI LANCIO PGM
009700001016     D*-------------------
009800060829     D PARAM           DS                  INZ
009900060829     D  PAROPE                        1
010000060829     D  PARDAI                        8  0
010100060829     D  PARDAF                        8  0
010200060901     D  PARIAD                        8  0
010300060901     D  PARFAD                        8  0
010400060830     D  PARGIO                        3  0
010500060830     D  PARDET                        1
010600061027     D  PARORD                        1
010700060830     D  PARKG1DA                      7  1
010800060830     D  PARKG1A                       7  1
010900060830     D  PARKG2DA                      7  1
011000060830     D  PARKG2A                       7  1
011100060830     D  PARKG3DA                      7  1
011200060830     D  PARKG3A                       7  1
011300060830     D  PARKG4DA                      7  1
011400060830     D  PARKG4A                       7  1
011500060830     D  PARDIS                        1
011600060830     D  PARARE                        3  0
011700060830     D  PARFIL                        3  0
011800060830     D  PARCAD                        2
011900060830     D  PARCAA                        2
012000060830     D*-------------------
012100060830     D* VARIABILI D WRK
012200060830     D*-------------------
012300060925     D wS02NRR         S              4  0
012400000000     C*------------------------------------------------------------------------*
012500000000     C* MAIN LINE
012600000000     C*------------------------------------------------------------------------*
012700060830     C*
012800060830     C* Caricamento scaglioni di peso / volume
012900060830     C                   exsr      carspv
013000010417     C*
013100010417     C* POI INIZIALIZZO LA DS DI PASSAGGIO PARAMETRI
013200010417     C                   RESET                   PARAM
013300000000     C*
013400000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
013500000000IF  1C     ERRL00        IFEQ      '0'
013600000000     C*
013700000000     C* CICLO FINO A CHE:
013800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
013900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
014000000000DO  2C     F03L00        DOUEQ     '1'
014100000000     C     F12L00        OREQ      '1'
014200000000     C*
014300000000     C* EMETTE FORMATO VIDEO
014400000000     C                   EXSR      BUTFMR
014500000000     C*
014600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
014700000000     C                   EXSR      FUNCON
014800000000     C*
014900000000E   2C                   ENDDO
015000000000E   1C                   ENDIF
015100000000     C*
015200000000     C                   SETON                                        LR
015300000000     C*------------------------------------------------------------------------*
015400000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
015500000000     C*------------------------------------------------------------------------*
015600000000     C     BUTFMR        BEGSR
015700000000     C*
015800000000     C* EMETTE TESTATA
015900060830     C                   WRITE     ST47D01
016000000000     C*
016100000000     C* VISUALIZZA MESSAGGIO
016200000000     C                   EXSR      SNDMSG
016300000000     C*
016400000000     C* EMETTE FUNZIONI
016500060830     C                   WRITE     ST47D02
016600000000     C*
016700000000     C* EMETTE FORMATO VIDEO
016800060830     C                   EXFMT     ST47D03
016900000000     C*
017000000000     C* AZZERA FLAG DI ERRORE LIVELLO
017100000000     C                   MOVEL     '0'           ERRL00
017200000000     C*
017300000000     C* AZZERA INDICATORI DI ERRORE
017400060830     C                   SETOFF                                       707172
017500060830     C                   SETOFF                                       808182
017600060830     C                   SETOFF                                       838485
017700060830     C                   SETOFF                                       8687
017800060830     C                   SETOFF                                       909192
017900060831     C                   SETOFF                                       9394
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* SNDMSG - MANDA MESSAGGIO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     SNDMSG        BEGSR
018600000000     C*
018700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
018800000000     C                   MOVEL     '1'           *IN40
018900060830     C                   WRITE     ST47DMC
019000000000     C                   MOVEL     '0'           *IN40
019100000000     C*
019200000000     C* VISUALIZZAZIONE MESSAGGIO
019300000000     C                   Z-ADD     0             MSGULT            4 0
019400000000     C                   CALL      'XMSG'        XMSGPA
019500000000     C     DSMSER        IFEQ      0
019600000000     C                   MOVEL     DSMSKE        MSGKEY
019700000000     C                   ENDIF
019800000000     C                   Z-ADD     1             MSGULT
019900060830     C                   WRITE     ST47DMS                                      SFL MESS
020000060830     C                   WRITE     ST47DMC                                      CONTR MESS
020100000000     C*
020200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
020300000000     C                   MOVEL     *BLANKS       DSMSMS
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
020800000000     C*------------------------------------------------------------------------*
020900000000     C     ENTGES        BEGSR
021000000000     C*
021100000000     C* CONTROLLA I CAMPI IMMESSI
021200000000     C                   EXSR      DECODI
021300000000     C*
021400000000     C                   ENDSR
021500001106     C*------------------------------------------------------------------------*
021600060829     C* F01GES - GESTIONE TASTO FUNZIONALE F01=AIUTO
021700001106     C*------------------------------------------------------------------------*
021800001106     C     F01GES        BEGSR
021900060830     C*
022000060830     C                   movel     '0'           wrksfl02          1
022100060830
022200060830      /free
022300060830
022400060830       // pulisco il subfile
022500060830        exsr sr_pulsfl;
022600060830
022700060830       // carico il subfile con i dati che trovo o vuoto
022800060830        exsr sr_carsfl;
022900060925
023000060925        S02NRR  = 1;
023100060925        wS02NRR = 1;
023200060830
023300060830        wrksfl02 = *on;
023400060830
023500060830        dou wrksfl02 = *off;
023600060830         write st47p02;
023700060830         exfmt st47c02;
023800060830
023900060830       // F12=Ritorno
024000060830          if *inkl;
024100060830           wrksfl02 = *off;
024200060830           leavesr;
024300060830          endif;
024400060830
024500060830         enddo;
024600060830
024700060830      /end-free
024800001106     C*
024900001106     C                   ENDSR
025000060830
025100000000     C*------------------------------------------------------------------------*
025200000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
025300000000     C*------------------------------------------------------------------------*
025400000000     C     F03GES        BEGSR
025500000000     C*
025600000000     C* IMPOSTA FLAG F03
025700000000     C                   MOVEL     '1'           F03L00
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
026200000000     C*------------------------------------------------------------------------*
026300000000     C     F06GES        BEGSR
026400000000     C*
026500000000     C* CONTROLLA I CAMPI IMMESSI
026600000000     C                   EXSR      DECODI
026700000000     C*
026800000000     C* LANCIA IL PROGRAMMA DI STAMPA
026900000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
027000000000     C*
027100000000     C* CARICA I TFP SELEZIONATI
027200000000     C                   EXSR      VIDPAR
027300010417     C                   MOVEL(p)  PARAM         KPJBU
027400060829     C                   MOVEL     'ST48'        KCOAZ
027500070118     C                   CALL      'BCH10'
027600070118 MOM C***                CALL      'TIST48C'
027700000000     C                   PARM                    KPJBA
027800060829     C*
027900060829     C* QUINDI ESCO DAL PROGRAMMA
028000060829     C                   MOVEL     '1'           F03L00
028100000000E   1C                   ENDIF
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*------------------------------------------------------------------------*
028500000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
028600000000     C*------------------------------------------------------------------------*
028700000000     C     F12GES        BEGSR
028800000000     C*
028900000000     C* IMPOSTO FLAG F12
029000000000     C                   MOVEL     '1'           F12L00
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
029500000000     C*------------------------------------------------------------------------*
029600000000     C     F24GES        BEGSR
029700000000     C*
029800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
029900000000     C                   EXSR      FUNGES
030000000000     C*
030100000000     C                   ENDSR
030200000000     C*------------------------------------------------------------------------*
030300001016     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030400000000     C*------------------------------------------------------------------------*
030500000000     C     DECODI        BEGSR
030600060830     C*
030700060830     C* AZZERA INDICATORI DI ERRORE
030800060830     C                   SETOFF                                       707172
030900060830     C                   SETOFF                                       808182
031000060830     C                   SETOFF                                       838485
031100060830     C                   SETOFF                                       8687
031200060830     C                   SETOFF                                       909192
031300060831     C                   SETOFF                                       9394
031400060830     C*--------------------
031500060830     C* DATA INIZIALE
031600060830     C*--------------------
031700060830     C     ERRL00        IFEQ      '0'
031800060830     C                   Z-ADD     VIDDTI        G08DAT
031900060830     C                   Z-ADD     *ZEROS        G08INV
032000060830     C                   MOVEL     '0'           G08ERR
032100060830     C                   CALL      'XSRDA8'
032200060830     C                   PARM                    WLBDA8
032300060830     C                   Z-ADD     G08DAT        VIDDTI
032400060830     C     G08ERR        IFEQ      '0'
032500060830     C                   Z-ADD     G08INV        WDTI              8 0
032600060830     C                   ELSE
032700060830     C                   MOVEL     '1'           ERRL00
032800060830     C                   MOVEL     MSG(16)       DSMSMS
032900060830     C                   SETON                                        70
033000060830     C                   ENDIF
033100060830     C                   ENDIF
033200060830     C*--------------------
033300060830     C* DATA FINALE
033400060830     C*--------------------
033500060830     C     ERRL00        IFEQ      '0'
033600060830     C                   Z-ADD     VIDDTF        G08DAT
033700060830     C                   Z-ADD     *ZEROS        G08INV
033800060830     C                   MOVEL     '0'           G08ERR
033900060830     C                   CALL      'XSRDA8'
034000060830     C                   PARM                    WLBDA8
034100060830     C                   Z-ADD     G08DAT        VIDDTF
034200060830     C     G08ERR        IFEQ      '0'
034300060830     C                   Z-ADD     G08INV        WDTF              8 0
034400060830     C                   ELSE
034500060830     C                   MOVEL     '1'           ERRL00
034600060830     C                   MOVEL     MSG(16)       DSMSMS
034700060830     C                   SETON                                        71
034800060830     C                   ENDIF
034900060830     C                   ENDIF
035000060830     C*--------------------
035100060830     C* CONGRUENZA LIMITI FASCE DI PESO
035200060830     C*--------------------
035300060830     C     ERRL00        IFEQ      '0'
035400060830     C                   IF        VIDKG1DA > *zeros OR VIDKG1A > *zeros
035500060830     C                   IF        VIDKG1DA >= VIDKG1A
035600060830     C                   MOVEL     '1'           ERRL00
035700060830     C                   MOVEL     MSG(9)        DSMSMS
035800060830     C                   SETON                                        8081
035900060830     C                   ENDIF
036000060830     C                   ENDIF
036100060830     C                   ENDIF
036200060830     C*
036300060830     C     ERRL00        IFEQ      '0'
036400060830     C                   IF        VIDKG2DA > *zeros OR VIDKG2A > *zeros
036500060830     C                   IF        VIDKG2DA >= VIDKG2A
036600060830     C                   MOVEL     '1'           ERRL00
036700060830     C                   MOVEL     MSG(9)        DSMSMS
036800060830     C                   SETON                                        8283
036900060830     C                   ENDIF
037000060830     C                   ENDIF
037100060830     C                   ENDIF
037200060830     C*
037300060830     C     ERRL00        IFEQ      '0'
037400060830     C                   IF        VIDKG3DA > *zeros OR VIDKG3A > *zeros
037500060830     C                   IF        VIDKG3DA >= VIDKG3A
037600060830     C                   MOVEL     '1'           ERRL00
037700060830     C                   MOVEL     MSG(9)        DSMSMS
037800060830     C                   SETON                                        8485
037900060830     C                   ENDIF
038000060830     C                   ENDIF
038100060830     C                   ENDIF
038200060830     C*
038300060830     C     ERRL00        IFEQ      '0'
038400060830     C                   IF        VIDKG4DA > *zeros OR VIDKG4A > *zeros
038500060830     C                   IF        VIDKG4DA >= VIDKG4A
038600060830     C                   MOVEL     '1'           ERRL00
038700060830     C                   MOVEL     MSG(9)        DSMSMS
038800060830     C                   SETON                                        8687
038900060830     C                   ENDIF
039000060830     C                   ENDIF
039100060830     C                   ENDIF
039200060830     C*--------------------
039300060830     C* OBBLIGATORIETA ALMENO 1 FASCIA DI PESO
039400060830     C*--------------------
039500060830     C     ERRL00        IFEQ      '0'
039600060830     C                   IF        VIDKG1A = *zeros AND
039700060830     C                             VIDKG2A = *zeros AND
039800060830     C                             VIDKG3A = *zeros AND
039900060830     C                             VIDKG4A = *zeros
040000060830     C                   MOVEL     '1'           ERRL00
040100060830     C                   MOVEL     MSG(10)       DSMSMS
040200060830     C                   SETON                                        808284
040300060830     C                   SETON                                        86
040400060830     C                   SETON                                        818385
040500060830     C                   SETON                                        87
040600060830     C                   ENDIF
040700060830     C                   ENDIF
040800060830     C*--------------------
040900060830     C* CONTROLLO LIMITI FASCE DI PESO
041000060830     C*--------------------
041100060830     C     ERRL00        IFEQ      '0'
041200060830     C                   IF        VIDKG1DA > *zeros
041300060830     C     VIDKG1DA      LOOKUP    SPVKG_DA                               55
041400060830     C                   IF        not %found
041500060830     C                   MOVEL     '1'           ERRL00
041600060830     C                   MOVEL     MSG(6)        DSMSMS
041700060830     C                   SETON                                        80
041800060830     C                   ENDIF
041900060830     C                   ENDIF
042000060830     C                   ENDIF
042100060830     C     ERRL00        IFEQ      '0'
042200060830     C                   IF        VIDKG1A > *zeros
042300060830     C     VIDKG1A       LOOKUP    SPVKG_A                                55
042400060830     C                   IF        not %found
042500060830     C                   MOVEL     '1'           ERRL00
042600060830     C                   MOVEL     MSG(7)        DSMSMS
042700060830     C                   SETON                                        81
042800060830     C                   ENDIF
042900060830     C                   ENDIF
043000060830     C                   ENDIF
043100060830     C*
043200060830     C     ERRL00        IFEQ      '0'
043300060830     C                   IF        VIDKG2DA > *zeros
043400060830     C     VIDKG2DA      LOOKUP    SPVKG_DA                               55
043500060830     C                   IF        not %found
043600060830     C                   MOVEL     '1'           ERRL00
043700060830     C                   MOVEL     MSG(6)        DSMSMS
043800060830     C                   SETON                                        82
043900060830     C                   ENDIF
044000060830     C                   ENDIF
044100060830     C                   ENDIF
044200060830     C     ERRL00        IFEQ      '0'
044300060830     C                   IF        VIDKG2A > *zeros
044400060830     C     VIDKG2A       LOOKUP    SPVKG_A                                55
044500060830     C                   IF        not %found
044600060830     C                   MOVEL     '1'           ERRL00
044700060830     C                   MOVEL     MSG(7)        DSMSMS
044800060830     C                   SETON                                        83
044900060830     C                   ENDIF
045000060830     C                   ENDIF
045100060830     C                   ENDIF
045200060830     C*
045300060830     C     ERRL00        IFEQ      '0'
045400060830     C                   IF        VIDKG3DA > *zeros
045500060830     C     VIDKG3DA      LOOKUP    SPVKG_DA                               55
045600060830     C                   IF        not %found
045700060830     C                   MOVEL     '1'           ERRL00
045800060830     C                   MOVEL     MSG(6)        DSMSMS
045900060830     C                   SETON                                        84
046000060830     C                   ENDIF
046100060830     C                   ENDIF
046200060830     C                   ENDIF
046300060830     C     ERRL00        IFEQ      '0'
046400060830     C                   IF        VIDKG3A > *zeros
046500060830     C     VIDKG3A       LOOKUP    SPVKG_A                                55
046600060830     C                   IF        not %found
046700060830     C                   MOVEL     '1'           ERRL00
046800060830     C                   MOVEL     MSG(7)        DSMSMS
046900060830     C                   SETON                                        85
047000060830     C                   ENDIF
047100060830     C                   ENDIF
047200060830     C                   ENDIF
047300060830     C*
047400060830     C     ERRL00        IFEQ      '0'
047500060830     C                   IF        VIDKG4DA > *zeros
047600060830     C     VIDKG4DA      LOOKUP    SPVKG_DA                               55
047700060830     C                   IF        not %found
047800060830     C                   MOVEL     '1'           ERRL00
047900060830     C                   MOVEL     MSG(6)        DSMSMS
048000060830     C                   SETON                                        86
048100060830     C                   ENDIF
048200060830     C                   ENDIF
048300060830     C                   ENDIF
048400060830     C     ERRL00        IFEQ      '0'
048500060830     C                   IF        VIDKG4A > *zeros
048600060830     C     VIDKG4A       LOOKUP    SPVKG_A                                55
048700060830     C                   IF        not %found
048800060830     C                   MOVEL     '1'           ERRL00
048900060830     C                   MOVEL     MSG(7)        DSMSMS
049000060830     C                   SETON                                        87
049100060830     C                   ENDIF
049200060830     C                   ENDIF
049300060830     C                   ENDIF
049400060831     C*--------------------
049500060831     C* SCELTA/CONTROLLO DISTRETTO
049600060831     C*--------------------
049700060831     C     ERRL00        IFEQ      '0'
049800060831     C                   IF        VIDDIS <> *blanks
049900060831     C                   MOVEL(P)  VIDDIS        WDIV              1
050000060831     C                   EXSR      CHKDIV
050100060831     C                   IF        ERRL00 = '1'
050200060831     C                   MOVEL     MSG(11)       DSMSMS
050300060831     C                   SETON                                        90
050400060831     C                   ENDIF
050500060831     C                   MOVEL(P)  WDIV          VIDDIS
050600060831     C                   ENDIF
050700060831     C                   ENDIF
050800060831     C*--------------------
050900060831     C* SCELTA/CONTROLLO AREA
051000060831     C*--------------------
051100060831     C     ERRL00        IFEQ      '0'
051200060831     C                   IF        VIDARE <> *blanks
051300060831     C                   MOVEL(P)  VIDARE        WARE              3
051400060831     C                   EXSR      CHKARE
051500060831     C                   IF        ERRL00 = '1'
051600060831     C                   MOVEL     MSG(12)       DSMSMS
051700060831     C                   SETON                                        91
051800060831     C                   ENDIF
051900060831     C                   MOVEL(P)  WARE          VIDARE
052000060831     C                   ENDIF
052100060831     C                   ENDIF
052200060831     C*--------------------
052300060831     C* SCELTA/CONTROLLO P.O.
052400060831     C*--------------------
052500060831     C     ERRL00        IFEQ      '0'
052600060831     C                   IF        VIDFIL <> *blanks
052700060831     C                   MOVEL(P)  VIDFIL        WFIL              3
052800060831     C                   EXSR      CHKFIL
052900060831     C                   IF        ERRL00 = '1'
053000060831     C                   MOVEL     MSG(13)       DSMSMS
053100060831     C                   SETON                                        92
053200060831     C                   ENDIF
053300060831     C                   MOVEL(P)  WFIL          VIDFIL
053400060831     C                   ENDIF
053500060831     C                   ENDIF
053600060831     C*--------------------
053700060831     C* CONTROLLO CAP "DA - A"
053800060831     C*--------------------
053900060831     C     ERRL00        IFEQ      '0'
054000060831     C                   IF        VIDCAD = *blanks
054100060831     C                   EVAL      VIDCAD = '00'
054200060831     C                   ENDIF
054300060831     C*
054400060831     C                   IF        VIDCAA = *blanks
054500060831     C                   EVAL      VIDCAA = '99'
054600060831     C                   ENDIF
054700060831     C                   ENDIF
054800060831     C*
054900060831     C     ERRL00        IFEQ      '0'
055000060831     C                   IF        VIDCAD > VIDCAA
055100060831     C                   MOVEL     '1'           ERRL00
055200060831     C                   MOVEL     MSG(15)       DSMSMS
055300060831     C                   SETON                                        9394
055400060831     C                   ENDIF
055500060831     C                   ENDIF
055600060831     C*--------------------
055700060904     C* CONTROLLO "NO TUTTO" (SOLO SE NO UTENTE EDP*)
055800060831     C*--------------------
055900060831     C     ERRL00        IFEQ      '0'
056000060904     C                   IF        %subst(KNMUS:1:3) <> 'EDP'
056100060831     C                   IF        VIDDIS = *blanks AND
056200060831     C                             VIDARE = *blanks AND
056300060831     C                             VIDFIL = *blanks AND
056400060831     C                             (VIDCAD = *blanks OR VIDCAD = '00') AND
056500060831     C                             (VIDCAA = *blanks OR VIDCAA = '99')
056600060831     C                   MOVEL     '1'           ERRL00
056700060831     C                   MOVEL     MSG(14)       DSMSMS
056800060831     C                   SETON                                        909192
056900060831     C                   SETON                                        9394
057000060904     C                   ENDIF
057100060831     C                   ENDIF
057200060831     C                   ENDIF
057300060830     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
057700000000     C*------------------------------------------------------------------------*
057800000000     C     AZZVID        BEGSR
057900010417     C*
058000010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
058100060830     C                   CLEAR                   ST47D03
058200060830     C*
058300060830     C* CARICO LE DATE INIZIALE E FINALE DAL FILE
058400060830     C                   OPEN      tiala00f
058500060830     C                   READ      tiala00f
058600060830     C                   CLOSE     tiala00f
058700060830     C*
058800060830     C* Giro la data iniziale
058900060830     C                   Z-ADD     *zeros        G08DAT
059000060830     C                   Z-ADD     alaDAI        G08INV
059100060830     C                   MOVEL     '3'           G08ERR
059200060830     C                   CALL      'XSRDA8'
059300060830     C                   PARM                    WLBDA8
059400060830     C                   Z-ADD     G08DAT        VIDDTI
059500060830     C*
059600060830     C* Giro la data finale
059700060830     C                   Z-ADD     *zeros        G08DAT
059800060830     C                   Z-ADD     alaDAF        G08INV
059900060830     C                   MOVEL     '3'           G08ERR
060000060830     C                   CALL      'XSRDA8'
060100060830     C                   PARM                    WLBDA8
060200060830     C                   Z-ADD     G08DAT        VIDDTF
060300060830     C*
060400060830     C* GIORNI LAVORATIVI
060500060830     C                   CLEAR                   WL08
060600060830     C                   Z-ADD     alaDAI        L08DA
060700060830     C                   Z-ADD     alaDAF        L08A
060800060830     C                   CALL      'XSRLAV8'
060900060830     C                   PARM                    WL08
061000060830     C                   MOVE      L08L          VIDGIO
061100060830     C*
061200060830     C* IMPOSTO GLI ALTRI VALORI D DEFAULT
061300060830     C                   EVAL      VIDDET = 'S'
061400061027     C                   EVAL      VIDORD = 'C'
061500060830     C                   EVAL      VIDCAD = '00'
061600060830     C                   EVAL      VIDCAA = '99'
061700060831     C                   EVAL      VIDDISD = '(" "   = tutte)'
061800060831     C                   EVAL      VIDARED = '("   " = tutte)'
061900060831     C                   EVAL      VIDFILD = '("   " = tutti)'
062000000000     C*
062100000000     C                   ENDSR
062200000000     C*------------------------------------------------------------------------*
062300001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
062400000000     C*------------------------------------------------------------------------*
062500000000     C     VIDPAR        BEGSR
062600000000     C*
062700060830     C                   MOVEL     'S'           PAROPE                         * F = genera File
062800001016     C                   Z-ADD     WDTI          PARDAI
062900001016     C                   Z-ADD     WDTF          PARDAF
063000060901     C                   Z-ADD     VIDDTI        PARIAD
063100060901     C                   Z-ADD     VIDDTF        PARFAD
063200060830     C                   Z-ADD     VIDGIO        PARGIO
063300060830     C                   MOVEL     VIDDET        PARDET
063400061027     C                   MOVEL     VIDORD        PARORD
063500060830     C                   Z-ADD     VIDKG1DA      PARKG1DA
063600060830     C                   Z-ADD     VIDKG1A       PARKG1A
063700060830     C                   Z-ADD     VIDKG2DA      PARKG2DA
063800060830     C                   Z-ADD     VIDKG2A       PARKG2A
063900060830     C                   Z-ADD     VIDKG3DA      PARKG3DA
064000060830     C                   Z-ADD     VIDKG3A       PARKG3A
064100060830     C                   Z-ADD     VIDKG4DA      PARKG4DA
064200060830     C                   Z-ADD     VIDKG4A       PARKG4A
064300060830     C                   MOVEL     VIDDIS        PARDIS
064400060831     C                   MOVEL     VIDARE        PARARE
064500060831     C                   MOVEL     VIDFIL        PARFIL
064600060830     C                   MOVEL     VIDCAD        PARCAD
064700060830     C                   MOVEL     VIDCAA        PARCAA
064800001016     C*
064900000000     C                   ENDSR
065000060830       // ------------------------------------------------------------------------
065100060830       // pulisco il subfile delle descrizioni in lingua
065200060830       // ------------------------------------------------------------------------
065300060830
065400060830      /free
065500060830
065600060830       begsr sr_pulsfl;
065700060830
065800060925        clear ws02nrr;
065900060830        *in24 = *off;
066000060830        *in25 = *off;
066100060830        write st47c02;
066200060830        *in24 = *on;
066300060830        *in25 = *on;
066400060830
066500060830       endsr;
066600060830
066700060830       // ------------------------------------------------------------------------
066800060830       // carico il subfile delle descrizioni in lingua
066900060830       // ------------------------------------------------------------------------
067000060830       begsr sr_carsfl;
067100060830
067200060830        *in30 = *off;
067300060830       // carico la tabella "ALA"
067400060830         clear dala;
067500060830         tbecod = 'ALA';
067600060830         setll tbecod tntbe01l;
067700060830         if %found(tntbe01l);
067800060831         reade (tbecod) tntbe01l;
067900060830         dow not %eof(tntbe01l);
068000060830          if tbeatb = *blanks;
068100060830           dala = tbeuni;
068200060830           vidw02sgl = tbeke1;
068300060830           vidw02des = §aladesc;
068400060925           ws02nrr = ws02nrr + 1;
068500060830           write st47s02;
068600060830          endif;
068700060831          reade (tbecod) tntbe01l;
068800060830        enddo;
068900060830        endif;
069000060830
069100060830        *in30 = *on;
069200060830
069300060830       endsr;
069400060830
069500060830      /end-free
069600060830
069700060830     C*------------------------------------------------------------------------*
069800060830     C* CARSPV - CARICA GLI SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
069900060830     C*------------------------------------------------------------------------*
070000060830     C     CARSPV        BEGSR
070100060830     C*
070200060830     C* Inizializzo le schiere d memorizzazione
070300060830     C                   clear                   SPVKG_DA
070400060830     C                   clear                   SPVKG_A
070500060830     C*
070600060830     C                   z-add     0             k                 3 0
070700060830     C                   movel     'ALA'         tbeCOD
070800060830     C     tbeCOD        setll     tntbe01l
070900060830     C                   if        %equal(tntbe01l)
071000060830     C     tbeCOD        reade     tntbe01l
071100060830     C                   dow       not %eof(tntbe01l)
071200060830     C                   if        tbeATb = *blanks
071300060830     C                   add       1             k
071400060830     C                   eval      DALA = tbeUNI
071500060830     C                   eval      SPVKG_DA(k) = §alapesda
071600060830     C                   eval      SPVKG_A(k)  = §alapesa
071700060830     C                   endif
071800060830     C     tbeCOD        reade     tntbe01l
071900060830     C                   enddo
072000060830     C                   endif
072100060830     C*
072200060830     C                   ENDSR
072300060831     C*------------------------------------------------------------------------*
072400060831     C* CHKDIV - CONTROLLA LA DIVISIONE
072500060831     C*------------------------------------------------------------------------*
072600060831     C     CHKDIV        BEGSR
072700060831     C*
072800060831     C* RICHIESTA INTERROGAZIONE ELENCO
072900060831     C     '?'           SCAN      WDIV                                   99
073000060831     C     *IN99         IFEQ      *ON
073100060831     C                   Z-ADD     1             §KUT
073200060831     C                   MOVEL     '17'          §COD
073300060831     C                   MOVEL     *BLANKS       §KEY
073400060831     C                   MOVEL     *BLANKS       §DES
073500060831     C                   CALL      'X§TABER'
073600060831     C                   PARM                    §KUT              1 0
073700060831     C                   PARM                    §COD              2
073800060831     C                   PARM                    §KEY              8
073900060831     C                   PARM                    §DES             30
074000060831     C                   MOVEL     §KEY          WDIV
074100060831     C                   ENDIF
074200060831     C*
074300060831     C* CONTROLLA ESISTENZA CODICE
074400060831     C                   Z-ADD     1             tblKUT
074500060831     C                   MOVEL     '17'          tblCOD
074600060831     C                   MOVEL(P)  WDIV          tblKEY
074700060831     C     KEYtab_c      CHAIN     TABEL00F                           99
074800060831     C     *IN99         IFEQ      *ON
074900060831     C     tblFLG        OREQ      '*'
075000060831     C                   MOVEL     '1'           ERRL00
075100060831     C                   ELSE
075200060831     C                   MOVEL     tblUNI        VIDDISD
075300060831     C                   ENDIF
075400060831     C*
075500060831     C                   ENDSR
075600060831     C*------------------------------------------------------------------------*
075700060831     C* CHKARE - CONTROLLA L'AREA
075800060831     C*------------------------------------------------------------------------*
075900060831     C     CHKARE        BEGSR
076000060831     C*
076100060831     C* RICHIESTA INTERROGAZIONE ELENCO
076200060831     C     '?'           SCAN      WARE                                   99
076300060831     C     *IN99         IFEQ      *ON
076400060831     C                   Z-ADD     1             §KUT
076500060831     C                   MOVEL     '05'          §COD
076600060831     C                   MOVEL     *BLANKS       §KEY
076700060831     C                   MOVEL     *BLANKS       §DES
076800060831     C                   CALL      'X§TABER'
076900060831     C                   PARM                    §KUT              1 0
077000060831     C                   PARM                    §COD              2
077100060831     C                   PARM                    §KEY              8
077200060831     C                   PARM                    §DES             30
077300060831     C                   MOVEL     §KEY          WARE
077400060831     C                   ENDIF
077500060831     C*
077600060831     C* CARATTERI ALFABETICI NON VALIDI
077700060831     C     WARE          IFLT      *ZEROS
077800060831     C                   MOVEL     '1'           ERRL00
077900060831     C                   ELSE
078000060831     C*
078100060831     C* CONTROLLA ESISTENZA CODICE
078200060831     C                   Z-ADD     1             tblKUT
078300060831     C                   MOVEL     '05'          tblCOD
078400060831     C                   MOVEL(P)  WARE          tblKEY
078500060831     C     KEYtab_c      CHAIN     TABEL00F                           99
078600060831     C     *IN99         IFEQ      *ON
078700060831     C     tblFLG        OREQ      '*'
078800060831     C                   MOVEL     '1'           ERRL00
078900060831     C                   ELSE
079000061016     C                   MOVEL     tblUNI        DS05
079100061016     C                   MOVEL(P)  §05DES        VIDARED
079200061016     C     §05PCP        CHAIN     AZORG01L
079300061016     C                   IF        %found(AZORG01L)
079400061016     C                   EVAL      VIDDIS = ORGFL3
079500061016     C                   ENDIF
079600060831     C                   ENDIF
079700060831     C                   ENDIF
079800060831     C*
079900060831     C                   ENDSR
080000060831     C*------------------------------------------------------------------------*
080100060831     C* CHKFIL - CONTROLLA LA FILIALE
080200060831     C*------------------------------------------------------------------------*
080300060831     C     CHKFIL        BEGSR
080400060831     C*
080500060831     C* RICHIESTA INTERROGAZIONE ELENCO
080600060831     C     '?'           SCAN      WFIL                                   99
080700060831     C     *IN99         IFEQ      *ON
080800060831     C                   MOVEL     *BLANKS       §COD1
080900060831     C                   MOVE      *BLANKS       §DES1
081000060831     C                   MOVE      *BLANKS       §PRA
081100060831     C                   CALL      'TNSD24R'
081200060831     C                   PARM                    §COD1             3
081300060831     C                   PARM                    §TIP              1
081400060831     C                   PARM                    §DES1            25
081500060831     C                   PARM                    §PRA              3
081600060831     C                   MOVEL     §COD1         WFIL
081700060831     C                   ENDIF
081800060831     C*
081900060831     C* CARATTERI ALFABETICI NON VALIDI
082000060831     C     WFIL          IFLT      *ZEROS
082100060831     C                   MOVEL     '1'           ERRL00
082200060831     C                   ELSE
082300060831     C*
082400060831     C* CONTROLLA ESISTENZA CODICE
082500060831     C                   MOVEL     WFIL          orgFIL
082600060831     C     orgFIL        CHAIN     AZORG01L                           99
082700060831     C     *IN99         IFEQ      *ON
082800060831     C     orgFAG        OREQ      'V'
082900060831     C     orgFVA        ORNE      *BLANKS
083000060831     C                   MOVEL     '1'           ERRL00
083100060831     C                   ELSE
083200060831     C                   MOVEL     ORGDES        VIDFILD
083300061016     C                   EVAL      VIDDIS = ORGFL3
083400061016     C                   MOVE(P)   ORGCAR        VIDARE
083500061016     C                   MOVE(P)   ORGFIL        VIDFIL
083600060831     C                   ENDIF
083700060831     C                   ENDIF
083800060831     C*
083900060831     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* *INZSR - OOPERAZIONI INIZIALI
084200000000     C*------------------------------------------------------------------------*
084300000000     C     *INZSR        BEGSR
084400000000     C*--------------------
084500000000     C* RICEVIMENTO PARAMETRI
084600000000     C*--------------------
084700000000     C     *ENTRY        PLIST
084800000000     C                   PARM                    KPJBA
084900060831     C*--------------------
085000060831     C* CHIAVI DI LETTURA
085100060831     C*--------------------
085200060831     C*
085300060831     C* CHIAVE LETTURA TABEL00F - COMPLETA
085400060831     C     KEYtab_c      KLIST
085500060831     C                   KFLD                    tblKUT
085600060831     C                   KFLD                    tblCOD
085700060831     C                   KFLD                    tblKEY
085800000000     C*--------------------
085900060829     C* VARIABILI NN RIFERITE AL DATABASE
086000000000     C*--------------------
086100000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
086200000000     C*
086300000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
086400000000     C     XMSGPA        PLIST
086500000000     C                   PARM                    DSMSMS
086600000000     C                   PARM                    DSMSPG
086700000000     C                   PARM                    DSMSKE
086800000000     C                   PARM                    DSMSER
086900000000     C*
087000000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
087100000000     C                   CALL      'XKEY'
087200000000     C                   PARM                    DSKEY
087300000000     C*
087400000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
087500000000     C*   -NOME PGM CHIAMANTE
087600000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
087700060830     C                   MOVEL     'TIST47R'     DSMSPG
087800060830     C                   MOVEL     'TIST47R'     MSGQUE
087900000000     C                   MOVEL     *BLANKS       MSGKEY
088000000000     C                   MOVEL     *BLANKS       DSMSMS
088100000000     C*
088200000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
088300000000     C                   Z-ADD     0             FUNGRU            5 0
088400000000     C*
088500000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
088600000000     C                   EXSR      FUNGES
088700000000     C*--------------------
088800000000     C* VARIABILI DI CONTROLLO
088900000000     C*--------------------
089000000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
089100000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
089200000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
089300000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
089400000000     C*
089500000000     C* PULISCE IL VIDEO DA EMETTERE
089600000000     C                   EXSR      AZZVID
089700000000     C*
089800000000     C                   ENDSR
089900000000     C*------------------------------------------------------------------------*
090000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
090100000000     C*------------------------------------------------------------------------*
090200000000     C     FUNGES        BEGSR
090300000000     C*
090400000000     C* PULISCO CAMPI DI LAVORO
090500000000     C                   Z-ADD     0             W                 5 0
090600000000     C*
090700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
090900000000     C     1             DO        24            I
091000000000     C                   MOVEL     FUD(I)        FUDRIG
091100000000     C*
091200000000     C* PULIZIA FUNZIONI PRECEDENTI
091300000000     C                   MOVEL     *BLANKS       FUV(I)
091400000000     C*
091500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
091600000000     C* (SALTA F24)
091700000000     C     FUDATT        IFEQ      'S'
091800000000     C     FUDFUN        ANDNE     '24'
091900000000     C                   ADD       1             W
092000000000     C                   MOVEL     FUDDES        FUV(W)
092100000000     C                   END
092200000000     C                   END                                                    DO WHILE
092300000000     C*
092400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
092500000000     C* ALTRIMENTI DISATTIVO F24
092600000000     C     1             DO        24            I
092700000000     C                   MOVEL     FUD(I)        FUDRIG
092800000000     C*
092900000000     C     FUDFUN        IFEQ      '24'
093000000000     C     W             IFGT      6
093100000000     C                   MOVEL     'S'           FUDATT
093200000000     C                   ELSE
093300000000     C                   MOVEL     *BLANKS       FUDATT
093400000000     C                   END                                                    W GT 8
093500000000     C                   MOVEL     FUDRIG        FUD(I)
093600000000     C                   END
093700000000     C                   END                                                    DO WHILE
093800000000     C*
093900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
094000000000     C     W             DIV       6             I
094100000000     C                   MVR                     Z
094200000000     C*
094300000000     C     Z             IFNE      0
094400000000     C     I             ADD       1             Z
094500000000     C                   ELSE
094600000000     C                   Z-ADD     I             Z                 5 0
094700000000     C                   END
094800000000     C*
094900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
095000000000     C                   ADD       1             FUNGRU
095100000000     C     FUNGRU        IFGT      Z
095200000000     C     FUNGRU        ORLT      1
095300000000     C                   Z-ADD     1             FUNGRU
095400000000     C                   END
095500000000     C*
095600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
095700000000     C     FUNGRU        IFEQ      1
095800000000     C                   MOVEL     FUV(1)        FUN001
095900000000     C                   MOVEL     FUV(2)        FUN002
096000000000     C                   MOVEL     FUV(3)        FUN003
096100000000     C                   MOVEL     FUV(4)        FUN004
096200000000     C                   MOVEL     FUV(5)        FUN005
096300000000     C                   MOVEL     FUV(6)        FUN006
096400000000     C                   END
096500000000     C*
096600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
096700000000     C     FUNGRU        IFEQ      2
096800000000     C                   MOVEL     FUV(7)        FUN001
096900000000     C                   MOVEL     FUV(8)        FUN002
097000000000     C                   MOVEL     FUV(9)        FUN003
097100000000     C                   MOVEL     FUV(10)       FUN004
097200000000     C                   MOVEL     FUV(11)       FUN005
097300000000     C                   MOVEL     FUV(12)       FUN006
097400000000     C                   END
097500000000     C*
097600000000     C* TERZO GRUPPO DI 6 FUNZIONI
097700000000     C     FUNGRU        IFEQ      3
097800000000     C                   MOVEL     FUV(13)       FUN001
097900000000     C                   MOVEL     FUV(14)       FUN002
098000000000     C                   MOVEL     FUV(15)       FUN003
098100000000     C                   MOVEL     FUV(16)       FUN004
098200000000     C                   MOVEL     FUV(17)       FUN005
098300000000     C                   MOVEL     FUV(18)       FUN006
098400000000     C                   END
098500000000     C*
098600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
098700000000     C     FUNGRU        IFEQ      4
098800000000     C                   MOVEL     FUV(19)       FUN001
098900000000     C                   MOVEL     FUV(20)       FUN002
099000000000     C                   MOVEL     FUV(21)       FUN003
099100000000     C                   MOVEL     FUV(22)       FUN004
099200000000     C                   MOVEL     *BLANKS       FUN005
099300000000     C                   MOVEL     *BLANKS       FUN006
099400000000     C                   END
099500000000     C*
099600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099700000000     C                   MOVEL     *BLANKS       FUN007
099800000000     C                   MOVEL     *BLANKS       FUN008
099900000000     C*
100000000000     C     1             DO        24            I
100100000000     C                   MOVEL     FUD(I)        FUDRIG
100200000000     C     FUDFUN        IFEQ      '24'
100300000000     C     FUDATT        ANDEQ     'S'
100400000000     C                   MOVEL     FUDDES        FUN008
100500000000     C                   END
100600000000     C                   END                                                    DO
100700000000     C*
100800000000     C                   ENDSR
100900000000     C*------------------------------------------------------------------------*
101000000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
101100000000     C*------------------------------------------------------------------------*
101200000000     C     FUNCON        BEGSR
101300000000     C*
101400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
101500000000     C                   EXSR      FUNVAR
101600000000     C*
101700000000     C* PULISCE CAMPI DI LAVORO
101800000000     C                   Z-ADD     0             I                 5 0
101900000000     C                   Z-ADD     0             W                 5 0
102000000000     C*
102100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
102200000000IF  1C     FUNKEY        IFNE      *BLANKS
102300000000DO  2C     1             DO        24            I
102400000000     C                   MOVEL     FUD(I)        FUDRIG
102500000000     C*
102600000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102700000000IF  3C     FUNKEY        IFEQ      FUDFUN
102800000000     C     FUDATT        ANDEQ     'S'
102900000000     C                   Z-ADD     I             W
103000000000E   3C                   ENDIF
103100000000E   2C                   ENDDO
103200000000     C*
103300000000     C* FUNZIONE NON ATTIVA, ERRORE
103400000000IF  2C     W             IFEQ      0
103500000000     C                   MOVEL     MSG(1)        DSMSMS
103600000000E   2C                   ENDIF
103700000000E   1C                   ENDIF
103800000000     C*
103900000000     C* FUNZIONE ATTIVA, PROCEDE
104000000000IF  1C     W             IFNE      0
104100000000     C     FUNKEY        OREQ      *BLANKS
104200000000IF  2C     FUNKEY        IFEQ      *BLANKS
104300000000     C                   EXSR      ENTGES
104400000000E   2C                   ENDIF
104500060829IF  2C     FUNKEY        IFEQ      '01'
104600060829     C                   EXSR      F01GES                                       *FINE LAVORO
104700060829E   2C                   ENDIF
104800000000IF  2C     FUNKEY        IFEQ      '03'
104900000000     C                   EXSR      F03GES                                       *FINE LAVORO
105000000000E   2C                   ENDIF
105100000000IF  2C     FUNKEY        IFEQ      '06'
105200000000     C                   EXSR      F06GES                                       *LANCIO
105300000000E   2C                   ENDIF
105400000000IF  2C     FUNKEY        IFEQ      '12'
105500000000     C                   EXSR      F12GES                                       *RITORNO
105600000000E   2C                   ENDIF
105700000000IF  2C     FUNKEY        IFEQ      '24'
105800000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
105900000000E   2C                   ENDIF
106000000000E   1C                   ENDIF
106100000000     C*
106200000000     C                   ENDSR
106300000000     C*------------------------------------------------------------------------*
106400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
106500000000     C*------------------------------------------------------------------------*
106600000000     C     FUNVAR        BEGSR
106700000000     C*
106800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
106900000000     C*
107000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
107100000000     C     KEY           IFEQ      ENTER
107200000000     C                   MOVEL     *BLANKS       FUNKEY
107300000000     C                   END
107400000000     C     KEY           IFEQ      F1
107500000000     C                   MOVEL     '01'          FUNKEY
107600000000     C                   END
107700000000     C     KEY           IFEQ      F2
107800000000     C                   MOVEL     '02'          FUNKEY
107900000000     C                   END
108000000000     C     KEY           IFEQ      F3
108100000000     C                   MOVEL     '03'          FUNKEY
108200000000     C                   END
108300000000     C     KEY           IFEQ      F4
108400000000     C                   MOVEL     '04'          FUNKEY
108500000000     C                   END
108600000000     C     KEY           IFEQ      F5
108700000000     C                   MOVEL     '05'          FUNKEY
108800000000     C                   END
108900000000     C     KEY           IFEQ      F6
109000000000     C                   MOVEL     '06'          FUNKEY
109100000000     C                   END
109200000000     C     KEY           IFEQ      F7
109300000000     C                   MOVEL     '07'          FUNKEY
109400000000     C                   END
109500000000     C     KEY           IFEQ      F8
109600000000     C                   MOVEL     '08'          FUNKEY
109700000000     C                   END
109800000000     C     KEY           IFEQ      F9
109900000000     C                   MOVEL     '09'          FUNKEY
110000000000     C                   END
110100000000     C     KEY           IFEQ      F10
110200000000     C                   MOVEL     '10'          FUNKEY
110300000000     C                   END
110400000000     C     KEY           IFEQ      F11
110500000000     C                   MOVEL     '11'          FUNKEY
110600000000     C                   END
110700000000     C     KEY           IFEQ      F12
110800000000     C                   MOVEL     '12'          FUNKEY
110900000000     C                   END
111000000000     C     KEY           IFEQ      F13
111100000000     C                   MOVEL     '13'          FUNKEY
111200000000     C                   END
111300000000     C     KEY           IFEQ      F14
111400000000     C                   MOVEL     '14'          FUNKEY
111500000000     C                   END
111600000000     C     KEY           IFEQ      F15
111700000000     C                   MOVEL     '15'          FUNKEY
111800000000     C                   END
111900000000     C     KEY           IFEQ      F16
112000000000     C                   MOVEL     '16'          FUNKEY
112100000000     C                   END
112200000000     C     KEY           IFEQ      F17
112300000000     C                   MOVEL     '17'          FUNKEY
112400000000     C                   END
112500000000     C     KEY           IFEQ      F18
112600000000     C                   MOVEL     '18'          FUNKEY
112700000000     C                   END
112800000000     C     KEY           IFEQ      F19
112900000000     C                   MOVEL     '19'          FUNKEY
113000000000     C                   END
113100000000     C     KEY           IFEQ      F20
113200000000     C                   MOVEL     '20'          FUNKEY
113300000000     C                   END
113400000000     C     KEY           IFEQ      F21
113500000000     C                   MOVEL     '21'          FUNKEY
113600000000     C                   END
113700000000     C     KEY           IFEQ      F22
113800000000     C                   MOVEL     '22'          FUNKEY
113900000000     C                   END
114000000000     C     KEY           IFEQ      F23
114100000000     C                   MOVEL     '23'          FUNKEY
114200000000     C                   END
114300000000     C     KEY           IFEQ      F24
114400000000     C                   MOVEL     '24'          FUNKEY
114500000000     C                   END
114600000000     C     KEY           IFEQ      ROLLUP
114700000000     C                   MOVEL     'RU'          FUNKEY
114800000000     C                   END
114900000000     C     KEY           IFEQ      HELP
115000000000     C                   MOVEL     'HP'          FUNKEY
115100000000     C                   END
115200000000     C                   ENDSR
115300000000     C*------------------------------------------------------------------------*
115400000000     C* NOTE PER COMPILAZIONE SCHIERE
115500000000     C*------------------------------------------------------------------------*
115600000000     C* -MSG-
115700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
115800000000     C* (NON TOCCARE MSG,1)
115900000000     C*
116000000000     C* -CMD-
116100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
116200000000     C*
116300000000     C* -FUD-
116400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
116500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
116600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
116700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
116800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
116900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
117000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
117100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
117200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
117300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
117400000000     C*
117500000000     C*------------------------------------------------------------------------*
117600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
117700060830TIST47R- Tasto funzionale non gestito                                          1
117800060830TIST47R- Distretto non valido                                                  2
117900060830TIST47R- Area non valida                                                       3
118000060830TIST47R- P.O. non valido                                                       4
118100060830TIST47R- Limite fascia errato (F01 per consultazione scaglioni)                5
118200060830TIST47R- Limite "DA" non valido (F01 per consultazione scaglioni)              6
118300060830TIST47R- Limite "A" non valido (F01 per consultazione scaglioni)               7
118400060830TIST47R- Inserire i giorni lavorativi                                          8
118500060831TIST47R- Limite "DA" maggiore limite "A"                                       9
118600060830TIST47R- Inserire almeno 1 fascia di peso                                     10
118700060831TIST47R- Distretto non valido                                                 11
118800060831TIST47R- Area non valida                                                      12
118900060831TIST47R- P.O. non valido                                                      13
119000060831TIST47R- Effettuare almeno una selezione                                      14
119100060831TIST47R- CAP "DAL" maggiore CAP "AL"                                          15
119200060830TIST47R- Data non valida                                                      16
119300060830TIST47R-                                                                      17
119400060830TIST47R-                                                                      18
119500060830TIST47R-                                                                      19
119600060830TIST47R-                                                                      20
119700000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
119800060829F01=Aiuto          -        (S/N)=S  <== S-FISSO           funz.01¬
119900000000                   -        (S/N)=N                        funz.02¬
120000000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
120100000000                   -        (S/N)=N                        funz.04¬
120200000000                   -        (S/N)=N                        funz.05¬
120300060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
120400000000                   -        (S/N)=N                        funz.07¬
120500000000                   -        (S/N)=N                        funz.08¬
120600000000                   -        (S/N)=N                        funz.09¬
120700060829                   -        (S/N)=N                        funz.10¬
120800000000                   -        (S/N)=N                        funz.11¬
120900060830F12=Ritorno        -        (S/N)=N                        funz.12¬
121000000000                   -        (S/N)=N                        funz.13¬
121100000000                   -        (S/N)=N                        funz.14¬
121200000000                   -        (S/N)=N                        funz.15¬
121300000000                   -        (S/N)=N                        funz.16¬
121400000000                   -        (S/N)=N                        funz.17¬
121500000000                   -        (S/N)=N                        funz.18¬
121600000000                   -        (S/N)=N                        funz.19¬
121700000000                   -        (S/N)=N                        funz.20¬
121800000000                   -        (S/N)=N                        funz.21¬
121900060829                   -        (S/N)=N                        funz.22¬
122000000000                   -        (S/N)=N                        funz.23¬
122100000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

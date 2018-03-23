000100000000     H*------------------------------------------------------------------------*
000200060829     H* CONTROLLO ASSORBIMENTI X LNA
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800060829     FTIST46D   CF   E             WORKSTN INFDS(DEVDS1)
000900060829     F                                     SFILE(ST46DMS:MSGULT)
001000060925     F                                     SFILE(ST46S02:wS02NRR)
001100060830     FTNTBE01L  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* DESCRIZIONI TASTI FUNZIONALI
002100000000     D*-------------------
002200000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002500000000     D*-------------------
002600000000     D FUV             S             19    DIM(24)
002700000000     D*-------------------
002800000000     D*------------------------------------------------------------------------*
002900000000     D* INPUT
003000000000     D*------------------------------------------------------------------------*
003100000000     D*-------------------
003200000000     D* RIDEFINIZIONE SCHIERA -FUD-
003300000000     D*-------------------
003400000000     D FUDDS           DS
003500000000     D*                -DESCRIZIONE PARZIALE
003600000000     D  FUDDE1                 1      1
003700000000     D*                -FUNZIONE
003800000000     D  FUDFUN                 2      3
003900000000     D*                -DESCRIZIONE PARZIALE
004000000000     D  FUDDE2                 4     19
004100000000     D*                -FILLER
004200000000     D  FUDFI0                20     34
004300000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004400000000     D  FUDATT                35     35
004500000000     D*                -FILLER
004600000000     D  FUDFI1                36     80
004700000000     D*                -DESCRIZIONE COMPLETA
004800000000     D  FUDDES                 1     19
004900000000     D*                -RIDEFINIZIONE RIGA
005000000000     D  FUDRIG                 1     80
005100000000     D*-------------------
005200000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005300000000     D*-------------------
005400000000     D DSMSG         E DS
005500000000     D*-------------------
005600000000     D* DS GESTIONE TASTI FUNZIONALI
005700000000     D*-------------------
005800000000     D DSKEY         E DS
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D*-------------------
006200000000     D* DS "XSRDA8" - CONTROLLO DATA(8)
006300000000     D*-------------------
006400971128     D WLBDA8          DS
006500000000     D  G08DAT                 1      8  0
006600000000     D  G08INV                 9     16  0
006700000000     D  G08ERR                17     17
006800000000     D  G08TGI                18     22  0
006900060830     D*-------------------
007000060830     D* DS SCAGLIONI PESO / VOLUME
007100060830     D*-------------------
007200060830     D DALA          E DS
007300060830     D*-------------------
007400000000     D* ARCHITETTURA
007500000000     D*-------------------
007600000000     D KPJBA         E DS
007700001016     D*-------------------
007800001016     D* DS PARAMETRI DI LANCIO PGM
007900001016     D*-------------------
008000060829     D PARAM           DS                  INZ
008100060829     D  PAROPE                        1
008200060829     D  PARDAI                        8  0
008300060829     D  PARDAF                        8  0
008400060829     D  PARDRI                        8  0
008500060830     D*-------------------
008600060830     D* VARIABILI D WRK
008700060830     D*-------------------
008800060925     D wS02NRR         S              4  0
008900000000     C*------------------------------------------------------------------------*
009000000000     C* MAIN LINE
009100000000     C*------------------------------------------------------------------------*
009200010417     C*
009300010417     C* POI INIZIALIZZO LA DS DI PASSAGGIO PARAMETRI
009400010417     C                   RESET                   PARAM
009500000000     C*
009600000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009700000000IF  1C     ERRL00        IFEQ      '0'
009800000000     C*
009900000000     C* CICLO FINO A CHE:
010000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
010100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
010200000000DO  2C     F03L00        DOUEQ     '1'
010300000000     C     F12L00        OREQ      '1'
010400000000     C*
010500000000     C* EMETTE FORMATO VIDEO
010600000000     C                   EXSR      BUTFMR
010700000000     C*
010800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010900000000     C                   EXSR      FUNCON
011000000000     C*
011100000000E   2C                   ENDDO
011200000000E   1C                   ENDIF
011300000000     C*
011400000000     C                   SETON                                        LR
011500000000     C*------------------------------------------------------------------------*
011600000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
011700000000     C*------------------------------------------------------------------------*
011800000000     C     BUTFMR        BEGSR
011900000000     C*
012000000000     C* EMETTE TESTATA
012100060829     C                   WRITE     ST46D01
012200000000     C*
012300000000     C* VISUALIZZA MESSAGGIO
012400000000     C                   EXSR      SNDMSG
012500000000     C*
012600000000     C* EMETTE FUNZIONI
012700060829     C                   WRITE     ST46D02
012800000000     C*
012900000000     C* EMETTE FORMATO VIDEO
013000060829     C                   EXFMT     ST46D03
013100000000     C*
013200000000     C* AZZERA FLAG DI ERRORE LIVELLO
013300000000     C                   MOVEL     '0'           ERRL00
013400000000     C*
013500000000     C* AZZERA INDICATORI DI ERRORE
013600060830     C                   SETOFF                                       707172
013700000000     C*
013800000000     C                   ENDSR
013900000000     C*------------------------------------------------------------------------*
014000000000     C* SNDMSG - MANDA MESSAGGIO
014100000000     C*------------------------------------------------------------------------*
014200000000     C     SNDMSG        BEGSR
014300000000     C*
014400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014500000000     C                   MOVEL     '1'           *IN40
014600060829     C                   WRITE     ST46DMC
014700000000     C                   MOVEL     '0'           *IN40
014800000000     C*
014900000000     C* VISUALIZZAZIONE MESSAGGIO
015000000000     C                   Z-ADD     0             MSGULT            4 0
015100000000     C                   CALL      'XMSG'        XMSGPA
015200000000     C     DSMSER        IFEQ      0
015300000000     C                   MOVEL     DSMSKE        MSGKEY
015400000000     C                   ENDIF
015500000000     C                   Z-ADD     1             MSGULT
015600060829     C                   WRITE     ST46DMS                                      SFL MESS
015700060829     C                   WRITE     ST46DMC                                      CONTR MESS
015800000000     C*
015900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016000000000     C                   MOVEL     *BLANKS       DSMSMS
016100000000     C*
016200000000     C                   ENDSR
016300000000     C*------------------------------------------------------------------------*
016400000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016500000000     C*------------------------------------------------------------------------*
016600000000     C     ENTGES        BEGSR
016700000000     C*
016800000000     C* CONTROLLA I CAMPI IMMESSI
016900000000     C                   EXSR      DECODI
017000000000     C*
017100000000     C                   ENDSR
017200001106     C*------------------------------------------------------------------------*
017300060829     C* F01GES - GESTIONE TASTO FUNZIONALE F01=AIUTO
017400001106     C*------------------------------------------------------------------------*
017500001106     C     F01GES        BEGSR
017600060830     C*
017700060830     C                   movel     '0'           wrksfl02          1
017800060830
017900060830      /free
018000060830
018100060830       // pulisco il subfile
018200060830        exsr sr_pulsfl;
018300060830
018400060830       // carico il subfile con i dati che trovo o vuoto
018500060830        exsr sr_carsfl;
018600060830
018700060925        S02NRR  = 1;
018800060925        wS02NRR = 1;
018900060925
019000060830        wrksfl02 = *on;
019100060830
019200060830        dou wrksfl02 = *off;
019300060830         write st46p02;
019400060830         exfmt st46c02;
019500060830
019600060830       // F12=Ritorno
019700060830          if *inkl;
019800060830           wrksfl02 = *off;
019900060830           leavesr;
020000060830          endif;
020100060830
020200060830         enddo;
020300060830
020400060830      /end-free
020500001106     C*
020600001106     C                   ENDSR
020700060830
020800000000     C*------------------------------------------------------------------------*
020900000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
021000000000     C*------------------------------------------------------------------------*
021100000000     C     F03GES        BEGSR
021200000000     C*
021300000000     C* IMPOSTA FLAG F03
021400000000     C                   MOVEL     '1'           F03L00
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
021900000000     C*------------------------------------------------------------------------*
022000000000     C     F06GES        BEGSR
022100000000     C*
022200000000     C* CONTROLLA I CAMPI IMMESSI
022300000000     C                   EXSR      DECODI
022400000000     C*
022500000000     C* LANCIA IL PROGRAMMA DI STAMPA
022600000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
022700000000     C*
022800000000     C* CARICA I TFP SELEZIONATI
022900000000     C                   EXSR      VIDPAR
023000010417     C                   MOVEL(p)  PARAM         KPJBU
023100060829     C                   MOVEL     'ST48'        KCOAZ
023200060829     C                   CALL      'BCH10'
023300060829 MOM C*                  CALL      'TIST48C'
023400000000     C                   PARM                    KPJBA
023500060829     C*
023600060829     C* QUINDI ESCO DAL PROGRAMMA
023700060829     C                   MOVEL     '1'           F03L00
023800000000E   1C                   ENDIF
023900000000     C*
024000000000     C                   ENDSR
024100000000     C*------------------------------------------------------------------------*
024200000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
024300000000     C*------------------------------------------------------------------------*
024400000000     C     F12GES        BEGSR
024500000000     C*
024600000000     C* IMPOSTO FLAG F12
024700000000     C                   MOVEL     '1'           F12L00
024800000000     C*
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
025200000000     C*------------------------------------------------------------------------*
025300000000     C     F24GES        BEGSR
025400000000     C*
025500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
025600000000     C                   EXSR      FUNGES
025700000000     C*
025800000000     C                   ENDSR
025900000000     C*------------------------------------------------------------------------*
026000001016     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026100000000     C*------------------------------------------------------------------------*
026200000000     C     DECODI        BEGSR
026300060830     C*
026400060830     C* AZZERA INDICATORI DI ERRORE
026500060830     C                   SETOFF                                       707172
026600000000     C*--------------------
026700000000     C* DATA INIZIALE
026800000000     C*--------------------
026900001016     C     ERRL00        IFEQ      '0'
027000000000     C                   Z-ADD     VIDDTI        G08DAT
027100000000     C                   Z-ADD     *ZEROS        G08INV
027200000000     C                   MOVEL     '0'           G08ERR
027300000000     C                   CALL      'XSRDA8'
027400971128     C                   PARM                    WLBDA8
027500000000     C                   Z-ADD     G08DAT        VIDDTI
027600001016     C     G08ERR        IFEQ      '0'
027700000000     C                   Z-ADD     G08INV        WDTI              8 0
027800001016     C                   ELSE
027900000000     C                   MOVEL     '1'           ERRL00
028000000000     C                   MOVEL     MSG(16)       DSMSMS
028100060830     C                   SETON                                        70
028200001016     C                   ENDIF
028300001016     C                   ENDIF
028400000000     C*--------------------
028500000000     C* DATA FINALE
028600000000     C*--------------------
028700001016     C     ERRL00        IFEQ      '0'
028800000000     C                   Z-ADD     VIDDTF        G08DAT
028900000000     C                   Z-ADD     *ZEROS        G08INV
029000000000     C                   MOVEL     '0'           G08ERR
029100000000     C                   CALL      'XSRDA8'
029200971128     C                   PARM                    WLBDA8
029300000000     C                   Z-ADD     G08DAT        VIDDTF
029400001016     C     G08ERR        IFEQ      '0'
029500000000     C                   Z-ADD     G08INV        WDTF              8 0
029600001016     C                   ELSE
029700000000     C                   MOVEL     '1'           ERRL00
029800000000     C                   MOVEL     MSG(16)       DSMSMS
029900060830     C                   SETON                                        71
030000001016     C                   ENDIF
030100001016     C                   ENDIF
030200001110     C*--------------------
030300001110     C* DATA DI RIFERIMENTO
030400001110     C*--------------------
030500001110     C     ERRL00        IFEQ      '0'
030600001110     C     VIDDRI        ANDNE     *ZEROS
030700001110     C                   Z-ADD     VIDDRI        G08DAT
030800001110     C                   Z-ADD     *ZEROS        G08INV
030900001110     C                   MOVEL     '0'           G08ERR
031000001110     C                   CALL      'XSRDA8'
031100001110     C                   PARM                    WLBDA8
031200001110     C                   Z-ADD     G08DAT        VIDDRI
031300001110     C     G08ERR        IFEQ      '0'
031400001110     C                   Z-ADD     G08INV        WDRI              8 0
031500001110     C                   ELSE
031600001110     C                   MOVEL     '1'           ERRL00
031700001110     C                   MOVEL     MSG(16)       DSMSMS
031800060830     C                   SETON                                        72
031900001110     C                   ENDIF
032000001110     C                   ENDIF
032100000000     C*
032200000000     C                   ENDSR
032300000000     C*------------------------------------------------------------------------*
032400000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
032500000000     C*------------------------------------------------------------------------*
032600000000     C     AZZVID        BEGSR
032700010417     C*
032800010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
032900060829     C                   CLEAR                   ST46D03
033000000000     C*
033100000000     C                   ENDSR
033200000000     C*------------------------------------------------------------------------*
033300001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
033400000000     C*------------------------------------------------------------------------*
033500000000     C     VIDPAR        BEGSR
033600000000     C*
033700060829     C                   MOVEL     'F'           PAROPE                         * F = genera File
033800001016     C                   Z-ADD     WDTI          PARDAI
033900001016     C                   Z-ADD     WDTF          PARDAF
034000001110     C                   Z-ADD     WDRI          PARDRI
034100001016     C*
034200000000     C                   ENDSR
034300060830       // ------------------------------------------------------------------------
034400060830       // pulisco il subfile delle descrizioni in lingua
034500060830       // ------------------------------------------------------------------------
034600060830
034700060830      /free
034800060830
034900060830       begsr sr_pulsfl;
035000060830
035100060925        clear ws02nrr;
035200060830        *in24 = *off;
035300060830        *in25 = *off;
035400060830        write st46c02;
035500060830        *in24 = *on;
035600060830        *in25 = *on;
035700060830
035800060830       endsr;
035900060830
036000060830       // ------------------------------------------------------------------------
036100060830       // carico il subfile delle descrizioni in lingua
036200060830       // ------------------------------------------------------------------------
036300060830       begsr sr_carsfl;
036400060830
036500060830        *in30 = *off;
036600060830       // carico la tabella "ALA"
036700060830         clear dala;
036800060830         tbecod = 'ALA';
036900060830         setll tbecod tntbe01l;
037000060830         if %found(tntbe01l);
037100060830         reade(n) (tbecod) tntbe01l;
037200060830         dow not %eof(tntbe01l);
037300060830          if tbeatb = *blanks;
037400060830           dala = tbeuni;
037500060830           vidw02sgl = tbeke1;
037600060830           vidw02des = §aladesc;
037700060925           ws02nrr = ws02nrr + 1;
037800060830           write st46s02;
037900060830          endif;
038000060830          reade(n) (tbecod) tntbe01l;
038100060830        enddo;
038200060830        endif;
038300060830
038400060830        *in30 = *on;
038500060830
038600060830       endsr;
038700060830
038800060830      /end-free
038900060830
039000000000     C*------------------------------------------------------------------------*
039100000000     C* *INZSR - OOPERAZIONI INIZIALI
039200000000     C*------------------------------------------------------------------------*
039300000000     C     *INZSR        BEGSR
039400000000     C*--------------------
039500000000     C* RICEVIMENTO PARAMETRI
039600000000     C*--------------------
039700000000     C     *ENTRY        PLIST
039800000000     C                   PARM                    KPJBA
039900000000     C*--------------------
040000060829     C* VARIABILI NN RIFERITE AL DATABASE
040100000000     C*--------------------
040200000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
040300000000     C*
040400000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
040500000000     C     XMSGPA        PLIST
040600000000     C                   PARM                    DSMSMS
040700000000     C                   PARM                    DSMSPG
040800000000     C                   PARM                    DSMSKE
040900000000     C                   PARM                    DSMSER
041000000000     C*
041100000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
041200000000     C                   CALL      'XKEY'
041300000000     C                   PARM                    DSKEY
041400000000     C*
041500000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
041600000000     C*   -NOME PGM CHIAMANTE
041700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
041800060829     C                   MOVEL     'TIST46R'     DSMSPG
041900060829     C                   MOVEL     'TIST46R'     MSGQUE
042000000000     C                   MOVEL     *BLANKS       MSGKEY
042100000000     C                   MOVEL     *BLANKS       DSMSMS
042200000000     C*
042300000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
042400000000     C                   Z-ADD     0             FUNGRU            5 0
042500000000     C*
042600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
042700000000     C                   EXSR      FUNGES
042800000000     C*--------------------
042900000000     C* VARIABILI DI CONTROLLO
043000000000     C*--------------------
043100000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
043200000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
043300000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
043400000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
043500000000     C*
043600000000     C* PULISCE IL VIDEO DA EMETTERE
043700000000     C                   EXSR      AZZVID
043800000000     C*
043900000000     C                   ENDSR
044000000000     C*------------------------------------------------------------------------*
044100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
044200000000     C*------------------------------------------------------------------------*
044300000000     C     FUNGES        BEGSR
044400000000     C*
044500000000     C* PULISCO CAMPI DI LAVORO
044600000000     C                   Z-ADD     0             W                 5 0
044700000000     C*
044800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
045000000000     C     1             DO        24            I
045100000000     C                   MOVEL     FUD(I)        FUDRIG
045200000000     C*
045300000000     C* PULIZIA FUNZIONI PRECEDENTI
045400000000     C                   MOVEL     *BLANKS       FUV(I)
045500000000     C*
045600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
045700000000     C* (SALTA F24)
045800000000     C     FUDATT        IFEQ      'S'
045900000000     C     FUDFUN        ANDNE     '24'
046000000000     C                   ADD       1             W
046100000000     C                   MOVEL     FUDDES        FUV(W)
046200000000     C                   END
046300000000     C                   END                                                    DO WHILE
046400000000     C*
046500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
046600000000     C* ALTRIMENTI DISATTIVO F24
046700000000     C     1             DO        24            I
046800000000     C                   MOVEL     FUD(I)        FUDRIG
046900000000     C*
047000000000     C     FUDFUN        IFEQ      '24'
047100000000     C     W             IFGT      6
047200000000     C                   MOVEL     'S'           FUDATT
047300000000     C                   ELSE
047400000000     C                   MOVEL     *BLANKS       FUDATT
047500000000     C                   END                                                    W GT 8
047600000000     C                   MOVEL     FUDRIG        FUD(I)
047700000000     C                   END
047800000000     C                   END                                                    DO WHILE
047900000000     C*
048000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
048100000000     C     W             DIV       6             I
048200000000     C                   MVR                     Z
048300000000     C*
048400000000     C     Z             IFNE      0
048500000000     C     I             ADD       1             Z
048600000000     C                   ELSE
048700000000     C                   Z-ADD     I             Z                 5 0
048800000000     C                   END
048900000000     C*
049000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
049100000000     C                   ADD       1             FUNGRU
049200000000     C     FUNGRU        IFGT      Z
049300000000     C     FUNGRU        ORLT      1
049400000000     C                   Z-ADD     1             FUNGRU
049500000000     C                   END
049600000000     C*
049700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
049800000000     C     FUNGRU        IFEQ      1
049900000000     C                   MOVEL     FUV(1)        FUN001
050000000000     C                   MOVEL     FUV(2)        FUN002
050100000000     C                   MOVEL     FUV(3)        FUN003
050200000000     C                   MOVEL     FUV(4)        FUN004
050300000000     C                   MOVEL     FUV(5)        FUN005
050400000000     C                   MOVEL     FUV(6)        FUN006
050500000000     C                   END
050600000000     C*
050700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
050800000000     C     FUNGRU        IFEQ      2
050900000000     C                   MOVEL     FUV(7)        FUN001
051000000000     C                   MOVEL     FUV(8)        FUN002
051100000000     C                   MOVEL     FUV(9)        FUN003
051200000000     C                   MOVEL     FUV(10)       FUN004
051300000000     C                   MOVEL     FUV(11)       FUN005
051400000000     C                   MOVEL     FUV(12)       FUN006
051500000000     C                   END
051600000000     C*
051700000000     C* TERZO GRUPPO DI 6 FUNZIONI
051800000000     C     FUNGRU        IFEQ      3
051900000000     C                   MOVEL     FUV(13)       FUN001
052000000000     C                   MOVEL     FUV(14)       FUN002
052100000000     C                   MOVEL     FUV(15)       FUN003
052200000000     C                   MOVEL     FUV(16)       FUN004
052300000000     C                   MOVEL     FUV(17)       FUN005
052400000000     C                   MOVEL     FUV(18)       FUN006
052500000000     C                   END
052600000000     C*
052700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
052800000000     C     FUNGRU        IFEQ      4
052900000000     C                   MOVEL     FUV(19)       FUN001
053000000000     C                   MOVEL     FUV(20)       FUN002
053100000000     C                   MOVEL     FUV(21)       FUN003
053200000000     C                   MOVEL     FUV(22)       FUN004
053300000000     C                   MOVEL     *BLANKS       FUN005
053400000000     C                   MOVEL     *BLANKS       FUN006
053500000000     C                   END
053600000000     C*
053700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
053800000000     C                   MOVEL     *BLANKS       FUN007
053900000000     C                   MOVEL     *BLANKS       FUN008
054000000000     C*
054100000000     C     1             DO        24            I
054200000000     C                   MOVEL     FUD(I)        FUDRIG
054300000000     C     FUDFUN        IFEQ      '24'
054400000000     C     FUDATT        ANDEQ     'S'
054500000000     C                   MOVEL     FUDDES        FUN008
054600000000     C                   END
054700000000     C                   END                                                    DO
054800000000     C*
054900000000     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
055200000000     C*------------------------------------------------------------------------*
055300000000     C     FUNCON        BEGSR
055400000000     C*
055500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
055600000000     C                   EXSR      FUNVAR
055700000000     C*
055800000000     C* PULISCE CAMPI DI LAVORO
055900000000     C                   Z-ADD     0             I                 5 0
056000000000     C                   Z-ADD     0             W                 5 0
056100000000     C*
056200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
056300000000IF  1C     FUNKEY        IFNE      *BLANKS
056400000000DO  2C     1             DO        24            I
056500000000     C                   MOVEL     FUD(I)        FUDRIG
056600000000     C*
056700000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
056800000000IF  3C     FUNKEY        IFEQ      FUDFUN
056900000000     C     FUDATT        ANDEQ     'S'
057000000000     C                   Z-ADD     I             W
057100000000E   3C                   ENDIF
057200000000E   2C                   ENDDO
057300000000     C*
057400000000     C* FUNZIONE NON ATTIVA, ERRORE
057500000000IF  2C     W             IFEQ      0
057600000000     C                   MOVEL     MSG(1)        DSMSMS
057700000000E   2C                   ENDIF
057800000000E   1C                   ENDIF
057900000000     C*
058000000000     C* FUNZIONE ATTIVA, PROCEDE
058100000000IF  1C     W             IFNE      0
058200000000     C     FUNKEY        OREQ      *BLANKS
058300000000IF  2C     FUNKEY        IFEQ      *BLANKS
058400000000     C                   EXSR      ENTGES
058500000000E   2C                   ENDIF
058600060829IF  2C     FUNKEY        IFEQ      '01'
058700060829     C                   EXSR      F01GES                                       *FINE LAVORO
058800060829E   2C                   ENDIF
058900000000IF  2C     FUNKEY        IFEQ      '03'
059000000000     C                   EXSR      F03GES                                       *FINE LAVORO
059100000000E   2C                   ENDIF
059200000000IF  2C     FUNKEY        IFEQ      '06'
059300000000     C                   EXSR      F06GES                                       *LANCIO
059400000000E   2C                   ENDIF
059500000000IF  2C     FUNKEY        IFEQ      '12'
059600000000     C                   EXSR      F12GES                                       *RITORNO
059700000000E   2C                   ENDIF
059800000000IF  2C     FUNKEY        IFEQ      '24'
059900000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
060000000000E   2C                   ENDIF
060100000000E   1C                   ENDIF
060200000000     C*
060300000000     C                   ENDSR
060400000000     C*------------------------------------------------------------------------*
060500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060600000000     C*------------------------------------------------------------------------*
060700000000     C     FUNVAR        BEGSR
060800000000     C*
060900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
061000000000     C*
061100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
061200000000     C     KEY           IFEQ      ENTER
061300000000     C                   MOVEL     *BLANKS       FUNKEY
061400000000     C                   END
061500000000     C     KEY           IFEQ      F1
061600000000     C                   MOVEL     '01'          FUNKEY
061700000000     C                   END
061800000000     C     KEY           IFEQ      F2
061900000000     C                   MOVEL     '02'          FUNKEY
062000000000     C                   END
062100000000     C     KEY           IFEQ      F3
062200000000     C                   MOVEL     '03'          FUNKEY
062300000000     C                   END
062400000000     C     KEY           IFEQ      F4
062500000000     C                   MOVEL     '04'          FUNKEY
062600000000     C                   END
062700000000     C     KEY           IFEQ      F5
062800000000     C                   MOVEL     '05'          FUNKEY
062900000000     C                   END
063000000000     C     KEY           IFEQ      F6
063100000000     C                   MOVEL     '06'          FUNKEY
063200000000     C                   END
063300000000     C     KEY           IFEQ      F7
063400000000     C                   MOVEL     '07'          FUNKEY
063500000000     C                   END
063600000000     C     KEY           IFEQ      F8
063700000000     C                   MOVEL     '08'          FUNKEY
063800000000     C                   END
063900000000     C     KEY           IFEQ      F9
064000000000     C                   MOVEL     '09'          FUNKEY
064100000000     C                   END
064200000000     C     KEY           IFEQ      F10
064300000000     C                   MOVEL     '10'          FUNKEY
064400000000     C                   END
064500000000     C     KEY           IFEQ      F11
064600000000     C                   MOVEL     '11'          FUNKEY
064700000000     C                   END
064800000000     C     KEY           IFEQ      F12
064900000000     C                   MOVEL     '12'          FUNKEY
065000000000     C                   END
065100000000     C     KEY           IFEQ      F13
065200000000     C                   MOVEL     '13'          FUNKEY
065300000000     C                   END
065400000000     C     KEY           IFEQ      F14
065500000000     C                   MOVEL     '14'          FUNKEY
065600000000     C                   END
065700000000     C     KEY           IFEQ      F15
065800000000     C                   MOVEL     '15'          FUNKEY
065900000000     C                   END
066000000000     C     KEY           IFEQ      F16
066100000000     C                   MOVEL     '16'          FUNKEY
066200000000     C                   END
066300000000     C     KEY           IFEQ      F17
066400000000     C                   MOVEL     '17'          FUNKEY
066500000000     C                   END
066600000000     C     KEY           IFEQ      F18
066700000000     C                   MOVEL     '18'          FUNKEY
066800000000     C                   END
066900000000     C     KEY           IFEQ      F19
067000000000     C                   MOVEL     '19'          FUNKEY
067100000000     C                   END
067200000000     C     KEY           IFEQ      F20
067300000000     C                   MOVEL     '20'          FUNKEY
067400000000     C                   END
067500000000     C     KEY           IFEQ      F21
067600000000     C                   MOVEL     '21'          FUNKEY
067700000000     C                   END
067800000000     C     KEY           IFEQ      F22
067900000000     C                   MOVEL     '22'          FUNKEY
068000000000     C                   END
068100000000     C     KEY           IFEQ      F23
068200000000     C                   MOVEL     '23'          FUNKEY
068300000000     C                   END
068400000000     C     KEY           IFEQ      F24
068500000000     C                   MOVEL     '24'          FUNKEY
068600000000     C                   END
068700000000     C     KEY           IFEQ      ROLLUP
068800000000     C                   MOVEL     'RU'          FUNKEY
068900000000     C                   END
069000000000     C     KEY           IFEQ      HELP
069100000000     C                   MOVEL     'HP'          FUNKEY
069200000000     C                   END
069300000000     C                   ENDSR
069400000000     C*------------------------------------------------------------------------*
069500000000     C* NOTE PER COMPILAZIONE SCHIERE
069600000000     C*------------------------------------------------------------------------*
069700000000     C* -MSG-
069800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069900000000     C* (NON TOCCARE MSG,1)
070000000000     C*
070100000000     C* -CMD-
070200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070300000000     C*
070400000000     C* -FUD-
070500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
070600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
070900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
071100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
071200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
071300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
071400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
071500000000     C*
071600000000     C*------------------------------------------------------------------------*
071700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
071800060829TIST46R- Tasto funzionale non gestito                                          1
071900060829TIST46R-                                                                       2
072000060829TIST46R-                                                                       3
072100060829TIST46R-                                                                       4
072200060829TIST46R-                                                                       5
072300060829TIST46R-                                                                       6
072400060829TIST46R-                                                                       7
072500060829TIST46R- Inserire i giorni lavorativi                                          8
072600060829TIST46R-                                                                       9
072700060829TIST46R-                                                                      10
072800060829TIST46R-                                                                      11
072900060829TIST46R-                                                                      12
073000060829TIST46R-                                                                      13
073100060829TIST46R-                                                                      14
073200060829TIST46R-                                                                      15
073300060829TIST46R- Data non valida                                                      16
073400060829TIST46R-                                                                      17
073500060829TIST46R-                                                                      18
073600060829TIST46R-                                                                      19
073700060829TIST46R-                                                                      20
073800000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
073900060829F01=Aiuto          -        (S/N)=S  <== S-FISSO           funz.01¬
074000000000                   -        (S/N)=N                        funz.02¬
074100000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
074200000000                   -        (S/N)=N                        funz.04¬
074300000000                   -        (S/N)=N                        funz.05¬
074400060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
074500000000                   -        (S/N)=N                        funz.07¬
074600000000                   -        (S/N)=N                        funz.08¬
074700000000                   -        (S/N)=N                        funz.09¬
074800060829                   -        (S/N)=N                        funz.10¬
074900000000                   -        (S/N)=N                        funz.11¬
075000060830                   -        (S/N)=N                        funz.12¬
075100000000                   -        (S/N)=N                        funz.13¬
075200000000                   -        (S/N)=N                        funz.14¬
075300000000                   -        (S/N)=N                        funz.15¬
075400000000                   -        (S/N)=N                        funz.16¬
075500000000                   -        (S/N)=N                        funz.17¬
075600000000                   -        (S/N)=N                        funz.18¬
075700000000                   -        (S/N)=N                        funz.19¬
075800000000                   -        (S/N)=N                        funz.20¬
075900000000                   -        (S/N)=N                        funz.21¬
076000060829                   -        (S/N)=N                        funz.22¬
076100000000                   -        (S/N)=N                        funz.23¬
076200060830                   -        (S/N)=N                        funz.24¬

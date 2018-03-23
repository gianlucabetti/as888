000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300020418      * STAMPA TERMINAL STANDARD / ECCEZIONALI - RICHIESTA PARAMETRI
000400020418      *------------------------------------------------------------------------*
000500941109     F*------------------------------------------------------------------------*
000600941109     F* DATA BASE
000700941109     F*------------------------------------------------------------------------*
000800970703     FTISI19D   CF   E             WORKSTN INFDS(DEVDS1)
000900970703     F                                     SFILE(SI19DMS:MSGULT)
001000941109     D*------------------------------------------------------------------------*
001100941109     D* SCHIERE
001200970910     D*------------------------------------------------------------------------*
001300970910     D*-------------------
001400000000     D* MESSAGGI
001500970910     D*-------------------
001600930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700970910     D*-------------------
001800000000     D* COMANDI
001900970910     D*-------------------
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002100970910     D*-------------------
002200970910     D* DESCRIZIONE TASTI FUNZIONALI
002300970910     D*-------------------
002400000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002500970910     D*-------------------
002600970910     D* TASTI FUNZIONALI
002700970910     D*-------------------
002800000000     D FUV             S             19    DIM(24)
002900941109     D*------------------------------------------------------------------------*
003000941109     D* INPUT
003100941109     D*------------------------------------------------------------------------*
003200941109     D*----------------
003300000000     D* RIDEFINIZIONE SCHIERA -FUD-
003400941109     D*----------------
003500000000     D FUDDS           DS
003600000000     D  FUDDE1                 1      1
003700000000     D  FUDFUN                 2      3
003800000000     D  FUDDE2                 4     19
003900000000     D  FUDFI0                20     34
004000000000     D  FUDATT                35     35
004100000000     D  FUDFI1                36     80
004200000000     D  FUDDES                 1     19
004300000000     D  FUDRIG                 1     80
004400970910     D*----------------
004500970910     D* DS PER GESTIONE TASTI FUNZIONALI
004600970910     D*----------------
004700970910     D DSKEY         E DS
004800970910     D DEVDS1          DS
004900970910     D  KEY                  369    369
005000941109     D*----------------
005100970910     D* DS "XMSG" - GESTIONE MESSAGGI PGM
005200941109     D*----------------
005300930201     D DSMSG         E DS
005400970910     D*----------------
005500970910     D* DS CONTROLLO DATE
005600970910     D*----------------
005700970910     D WLBDA8          DS
005800970910     D  G08DAT                 1      8  0
005900970910     D  G08INV                 9     16  0
006000970910     D  G08ERR                17     17
006100970910     D  G08TGI                18     22  0
006200980206     D*------------
006300980206     D* DS DI PROCEDURA
006400980206     D*------------
006500980206     D DSSI00        E DS                  EXTNAME(TISI00DS)
006600980206     D*----------------
006700980206     D* DS PARAMETRI DI LANCIO
006800980206     D*----------------
006900980206     D PARAM           DS                  INZ
007000980206     D  PARDAT                 1      8  0
007100941109     D*----------------
007200930201     D* ARCHITETTURA
007300941109     D*----------------
007400930201     D KPJBA         E DS
007500941109     C*------------------------------------------------------------------------*
007600941109     C* MAIN LINE
007700941109     C*------------------------------------------------------------------------*
007800000000     C*
007900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
008100000000     C*
008200000000     C* CICLO FINO A CHE:
008300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008500930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
008600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
008700000000     C     F12L00        OREQ      '1'
008800930202     C     FLFINE        OREQ      '1'
008900000000     C*
009000000000     C* EMETTE FORMATO VIDEO
009100000000     C                   EXSR      BUTFMR
009200000000     C*
009300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
009400000000     C                   EXSR      FUNCON
009500000000     C*
009600000000     C                   END                                                    <-- 2 --
009700000000     C*
009800980206     C                   ELSE
009900980206     C                   MOVEL     '1'           D00ERR
010000980206     C                   MOVEL     DSMSMS        D00MSG
010100000000     C                   END                                                    <-- 1 --
010200000000     C*
010300000000     C                   SETON                                        LR
010400000000     C*================================================================
010500000000     C* EMISSIONE FORMATO A VIDEO
010600000000     C*================================================================
010700000000     C     BUTFMR        BEGSR
010800930204     C*
010900930204     C* WRITE RECORD TESTATA
011000970703     C                   WRITE     SI19D01
011100930205     C*
011200930205     C* VISUALIZZAZIONE MESSAGGIO
011300930205     C                   EXSR      SNDMSG
011400930204     C*
011500930204     C* WRITE RECORD FUNZIONI
011600970703     C                   WRITE     SI19D02
011700000000     C*
011800930204     C* EXFMT RECORD DETTAGLIO
011900970703     C                   EXFMT     SI19D03
012000000000     C*
012100000000     C* AZZERO FLAG DI ERRORE LIVELLO
012200000000     C                   MOVEL     '0'           ERRL00
012300000000     C*
012400000000     C* AZZERO FLAG DI ERRORE
012500950824     C                   SETOFF                                       82
012600000000     C*
012700000000     C                   ENDSR
012800000000     C*================================================================
012900000000     C* SNDMSG - MANDA MESSAGGIO
013000000000     C*================================================================
013100000000     C     SNDMSG        BEGSR
013200000000     C*
013300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
013400000000     C                   MOVEL     '1'           *IN40
013500970703     C                   WRITE     SI19DMC
013600000000     C                   MOVEL     '0'           *IN40
013700000000     C*
013800000000     C* VISUALIZZAZIONE MESSAGGIO
013900000000     C                   Z-ADD     0             MSGULT            4 0
014000930202     C                   CALL      'XMSG'        XMSGPA
014100930201     C     DSMSER        IFEQ      0
014200930201     C                   MOVEL     DSMSKE        MSGKEY
014300000000     C                   END
014400000000     C                   Z-ADD     1             MSGULT
014500970703     C                   WRITE     SI19DMS                                      SFL MESS
014600970703     C                   WRITE     SI19DMC                                      CONTR MESS
014700000000     C*
014800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
014900930201     C                   MOVEL     *BLANKS       DSMSMS
015000000000     C*
015100000000     C                   ENDSR
015200000000     C*================================================================
015300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
015400000000     C*================================================================
015500000000     C     ENTGES        BEGSR
015600930201     C*
015700930201     C* CONTROLLA I CAMPI IMMESSI
015800930201     C                   EXSR      DECODI
015900000000     C*
016000000000     C                   ENDSR
016100000000     C*================================================================
016200930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
016300000000     C*================================================================
016400000000     C     F01GES        BEGSR
016500000000     C                   ENDSR
016600000000     C*================================================================
016700930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
016800000000     C*================================================================
016900000000     C     F03GES        BEGSR
017000000000     C*
017100930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017200000000     C                   MOVEL     '1'           F03L00
017300980206     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
017400000000     C*
017500000000     C                   ENDSR
017600930201     C*================================================================
017700930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
017800930201     C*================================================================
017900930201     C     F06GES        BEGSR
018000960110     C*
018100960110     C* CONTROLLA I CAMPI IMMESSI
018200960110     C                   EXSR      DECODI
018300930201     C*
018400930208     C* LANCIO IL PROGRAMMA DI STAMPA
018500960110IF  1C     ERRL00        IFEQ      '0'
018600930208     C                   EXSR      VIDPAR
018700970910     C                   MOVEL     PARAM         KPJBU
018800970910     C                   MOVEL     'SI20'        KCOAZ
018900020603     C**                 CALL      'BCH10'
019000020603     C                   CALL      'TISI20R'
019100930208     C                   PARM                    KPJBA
019200970910     C                   MOVEL     '1'           FLFINE
019300960110E   1C                   ENDIF
019400930201     C*
019500930201     C                   ENDSR
019600000000     C*================================================================
019700930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
019800000000     C*================================================================
019900000000     C     F07GES        BEGSR
020000000000     C                   ENDSR
020100000000     C*================================================================
020200930208     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
020300000000     C*================================================================
020400000000     C     F08GES        BEGSR
020500000000     C                   ENDSR
020600000000     C*================================================================
020700930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
020800000000     C*================================================================
020900000000     C     F12GES        BEGSR
021000000000     C*
021100000000     C* IMPOSTO FLAG DI LIVELLO
021200000000     C                   MOVEL     '1'           F12L00
021300980206     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021400000000     C*
021500000000     C                   ENDSR
021600930201     C*================================================================
021700930208     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
021800930201     C*================================================================
021900930201     C     F21GES        BEGSR
022000930201     C                   ENDSR
022100000000     C*================================================================
022200930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
022300000000     C*================================================================
022400000000     C     F22GES        BEGSR
022500000000     C*
022600000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022700000000     C                   Z-ADD     80            LENGH            15 5
022800000000     C                   CALL      'QCAEXEC'
022900000000     C                   PARM                    CMD(1)
023000000000     C                   PARM                    LENGH
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*================================================================
023400930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023500000000     C*================================================================
023600000000     C     F24GES        BEGSR
023700000000     C*
023800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
023900000000     C                   EXSR      FUNGES
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*================================================================
024300000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
024400000000     C*================================================================
024500000000     C     HELGES        BEGSR
024600000000     C                   ENDSR
024700000000     C*================================================================
024800930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024900000000     C*================================================================
025000000000     C     DECODI        BEGSR
025100930720     C*
025200970910     C* DATA DI RIFERIMENTO
025300970910     C                   Z-ADD     VIDDDE        G08DAT
025400970910     C                   Z-ADD     *ZEROS        G08INV
025500970910     C                   MOVEL     '0'           G08ERR
025600950824     C                   CALL      'XSRDA8'
025700970910     C                   PARM                    WLBDA8
025800970910     C     G08ERR        IFEQ      '0'
025900970910     C                   Z-ADD     G08DAT        VIDDDE
026000970910     C                   Z-ADD     G08INV        WDDEI             8 0
026100950824     C                   ELSE
026200950824     C                   MOVEL     '1'           ERRL00
026300950824     C                   MOVEL     MSG(2)        DSMSMS
026400950824     C                   SETON                                        82
026500950824     C                   ENDIF
026600000000     C*
026700000000     C                   ENDSR
026800930201     C*================================================================
026900930208     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
027000930201     C*================================================================
027100930208     C     VIDPAR        BEGSR
027200930720     C*
027300950824     C* DATA RIFERIMENTO
027400950824     C                   Z-ADD     WDDEI         PARDAT
027500930201     C*
027600930201     C                   ENDSR
027700000000     C*================================================================
027800970910     C* *INZSR - OPERAZIONI INIZIALI
027900000000     C*================================================================
028000970910     C     *INZSR        BEGSR
028100930208     C*
028200930208     C* RICEVIMENTO PARAMETRI
028300930208     C     *ENTRY        PLIST
028400930208     C                   PARM                    KPJBA
028500980206     C                   PARM                    DSSI00
028600930201     C*
028700930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
028800930201     C     XMSGPA        PLIST
028900930201     C                   PARM                    DSMSMS
029000930201     C                   PARM                    DSMSPG
029100930201     C                   PARM                    DSMSKE
029200930201     C                   PARM                    DSMSER
029300930201     C*
029400930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
029500930201     C                   CALL      'XKEY'
029600930201     C                   PARM                    DSKEY
029700930201     C*
029800930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
029900930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
030000930201     C*
030100930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
030200930201     C*   -NOME PGM CHIAMANTE
030300930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
030400970703     C                   MOVEL     'TISI19R'     DSMSPG
030500970703     C                   MOVEL     'TISI19R'     MSGQUE
030600930201     C                   MOVEL     *BLANKS       MSGKEY
030700930201     C                   MOVEL     *BLANKS       DSMSMS
030800930201     C*
030900930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
031000930201     C                   Z-ADD     0             FUNGRU            5 0
031100930201     C*
031200930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
031300930201     C                   EXSR      FUNGES
031400930201     C*
031500930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
031600930201     C                   MOVEL     '0'           F12L00            1
031700930201     C                   MOVEL     '0'           F03L00            1
031800930201     C                   MOVEL     '0'           FINL00            1
031900930201     C                   MOVEL     '0'           ERRL00            1
032000930201     C                   MOVEL     '0'           FLFINE            1
032100930208     C*
032200930208     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
032300970910     C                   Z-ADD     UDATE         G08DAT
032400970910     C                   Z-ADD     *ZEROS        G08INV
032500970910     C                   MOVEL     '0'           G08ERR
032600950824     C                   CALL      'XSRDA8'
032700970910     C                   PARM                    WLBDA8
032800970910     C                   Z-ADD     G08DAT        VIDDDE
032900000000     C*
033000000000     C                   ENDSR
033100000000     C*================================================================
033200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
033300000000     C*================================================================
033400000000     C     FUNGES        BEGSR
033500000000     C*
033600000000     C* PULISCO CAMPI DI LAVORO
033700000000     C                   Z-ADD     0             W                 5 0
033800000000     C*
033900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
034100000000     C     1             DO        24            I
034200000000     C                   MOVEL     FUD(I)        FUDRIG
034300000000     C*
034400000000     C* PULIZIA FUNZIONI PRECEDENTI
034500000000     C                   MOVEL     *BLANKS       FUV(I)
034600000000     C*
034700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
034800000000     C* (SALTA F24)
034900000000     C     FUDATT        IFEQ      'S'
035000000000     C     FUDFUN        ANDNE     '24'
035100000000     C                   ADD       1             W
035200000000     C                   MOVEL     FUDDES        FUV(W)
035300000000     C                   END
035400000000     C                   END                                                    DO WHILE
035500000000     C*
035600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
035700000000     C* ALTRIMENTI DISATTIVO F24
035800000000     C     1             DO        24            I
035900000000     C                   MOVEL     FUD(I)        FUDRIG
036000000000     C*
036100000000     C     FUDFUN        IFEQ      '24'
036200000000     C     W             IFGT      6
036300000000     C                   MOVEL     'S'           FUDATT
036400000000     C                   ELSE
036500000000     C                   MOVEL     *BLANKS       FUDATT
036600000000     C                   END                                                    W GT 8
036700000000     C                   MOVEL     FUDRIG        FUD(I)
036800000000     C                   END
036900000000     C                   END                                                    DO WHILE
037000000000     C*
037100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
037200000000     C     W             DIV       6             I
037300000000     C                   MVR                     Z
037400000000     C*
037500000000     C     Z             IFNE      0
037600000000     C     I             ADD       1             Z
037700000000     C                   ELSE
037800000000     C                   Z-ADD     I             Z                 5 0
037900000000     C                   END
038000000000     C*
038100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
038200000000     C                   ADD       1             FUNGRU
038300000000     C     FUNGRU        IFGT      Z
038400000000     C     FUNGRU        ORLT      1
038500000000     C                   Z-ADD     1             FUNGRU
038600000000     C                   END
038700000000     C*
038800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
038900000000     C     FUNGRU        IFEQ      1
039000000000     C                   MOVEL     FUV(1)        FUN001
039100000000     C                   MOVEL     FUV(2)        FUN002
039200000000     C                   MOVEL     FUV(3)        FUN003
039300000000     C                   MOVEL     FUV(4)        FUN004
039400000000     C                   MOVEL     FUV(5)        FUN005
039500000000     C                   MOVEL     FUV(6)        FUN006
039600000000     C                   END
039700000000     C*
039800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
039900000000     C     FUNGRU        IFEQ      2
040000000000     C                   MOVEL     FUV(7)        FUN001
040100000000     C                   MOVEL     FUV(8)        FUN002
040200000000     C                   MOVEL     FUV(9)        FUN003
040300000000     C                   MOVEL     FUV(10)       FUN004
040400000000     C                   MOVEL     FUV(11)       FUN005
040500000000     C                   MOVEL     FUV(12)       FUN006
040600000000     C                   END
040700000000     C*
040800000000     C* TERZO GRUPPO DI 6 FUNZIONI
040900000000     C     FUNGRU        IFEQ      3
041000000000     C                   MOVEL     FUV(13)       FUN001
041100000000     C                   MOVEL     FUV(14)       FUN002
041200000000     C                   MOVEL     FUV(15)       FUN003
041300000000     C                   MOVEL     FUV(16)       FUN004
041400000000     C                   MOVEL     FUV(17)       FUN005
041500000000     C                   MOVEL     FUV(18)       FUN006
041600000000     C                   END
041700000000     C*
041800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
041900000000     C     FUNGRU        IFEQ      4
042000000000     C                   MOVEL     FUV(19)       FUN001
042100000000     C                   MOVEL     FUV(20)       FUN002
042200000000     C                   MOVEL     FUV(21)       FUN003
042300000000     C                   MOVEL     FUV(22)       FUN004
042400000000     C                   MOVEL     *BLANKS       FUN005
042500000000     C                   MOVEL     *BLANKS       FUN006
042600000000     C                   END
042700000000     C*
042800000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
042900000000     C                   MOVEL     *BLANKS       FUN007
043000000000     C                   MOVEL     *BLANKS       FUN008
043100000000     C*
043200000000     C     1             DO        24            I
043300000000     C                   MOVEL     FUD(I)        FUDRIG
043400000000     C     FUDFUN        IFEQ      '24'
043500000000     C     FUDATT        ANDEQ     'S'
043600000000     C                   MOVEL     FUDDES        FUN008
043700000000     C                   END
043800000000     C                   END                                                    DO
043900000000     C                   ENDSR
044000000000     C*================================================================
044100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
044200000000     C*================================================================
044300000000     C     FUNCON        BEGSR
044400000000     C*
044500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
044600000000     C                   EXSR      FUNVAR
044700000000     C*
044800000000     C* PULISCO CAMPI DI LAVORO
044900000000     C                   Z-ADD     0             I                 5 0
045000000000     C                   Z-ADD     0             W                 5 0
045100000000     C*
045200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
045300000000     C     FUNKEY        IFNE      *BLANKS
045400000000     C     1             DO        24            I
045500000000     C                   MOVEL     FUD(I)        FUDRIG
045600000000     C*
045700000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
045800000000     C     FUNKEY        IFEQ      FUDFUN
045900000000     C     FUDATT        ANDEQ     'S'
046000000000     C                   Z-ADD     I             W
046100000000     C                   END
046200000000     C                   END                                                    DO WHILE
046300000000     C*
046400000000     C* FUNZIONE NON ATTIVA (ERRORE)
046500000000     C     W             IFEQ      0
046600000000     C*
046700000000     C* EMISSIONE MESSAGGIO DI ERRORE
046800930201     C                   MOVEL     MSG(1)        DSMSMS
046900930201     C*
047000930201     C                   END                                                    W=0
047100000000     C                   END                                                    FUNKEY NE *BLANKS
047200000000     C*
047300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
047400000000     C     W             IFNE      0
047500000000     C     FUNKEY        OREQ      *BLANKS
047600000000     C*
047700000000     C     FUNKEY        IFEQ      *BLANKS
047800000000     C                   EXSR      ENTGES
047900000000     C                   END
048000000000     C     FUNKEY        IFEQ      '01'
048100000000     C                   EXSR      F01GES
048200000000     C                   END
048300000000     C     FUNKEY        IFEQ      '03'
048400000000     C                   EXSR      F03GES
048500000000     C                   END
048600930202     C     FUNKEY        IFEQ      '06'
048700930202     C                   EXSR      F06GES
048800930202     C                   END
048900000000     C     FUNKEY        IFEQ      '07'
049000000000     C                   EXSR      F07GES
049100000000     C                   END
049200000000     C     FUNKEY        IFEQ      '08'
049300000000     C                   EXSR      F08GES
049400000000     C                   END
049500000000     C     FUNKEY        IFEQ      '12'
049600000000     C                   EXSR      F12GES
049700000000     C                   END
049800930201     C     FUNKEY        IFEQ      '21'
049900930201     C                   EXSR      F21GES
050000930201     C                   END
050100000000     C     FUNKEY        IFEQ      '22'
050200000000     C                   EXSR      F22GES
050300000000     C                   END
050400000000     C     FUNKEY        IFEQ      '24'
050500000000     C                   EXSR      F24GES
050600000000     C                   END
050700000000     C     FUNKEY        IFEQ      'HP'
050800000000     C                   EXSR      HELGES
050900000000     C                   END
051000000000     C*
051100000000     C                   END
051200000000     C*
051300000000     C                   ENDSR
051400000000     C*================================================================
051500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051600000000     C*================================================================
051700000000     C     FUNVAR        BEGSR
051800000000     C*
051900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
052000000000     C*
052100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052200000000     C     KEY           IFEQ      ENTER
052300000000     C                   MOVEL     *BLANKS       FUNKEY
052400000000     C                   END
052500000000     C     KEY           IFEQ      F1
052600000000     C                   MOVEL     '01'          FUNKEY
052700000000     C                   END
052800000000     C     KEY           IFEQ      F2
052900000000     C                   MOVEL     '02'          FUNKEY
053000000000     C                   END
053100000000     C     KEY           IFEQ      F3
053200000000     C                   MOVEL     '03'          FUNKEY
053300000000     C                   END
053400000000     C     KEY           IFEQ      F4
053500000000     C                   MOVEL     '04'          FUNKEY
053600000000     C                   END
053700000000     C     KEY           IFEQ      F5
053800000000     C                   MOVEL     '05'          FUNKEY
053900000000     C                   END
054000000000     C     KEY           IFEQ      F6
054100000000     C                   MOVEL     '06'          FUNKEY
054200000000     C                   END
054300000000     C     KEY           IFEQ      F7
054400000000     C                   MOVEL     '07'          FUNKEY
054500000000     C                   END
054600000000     C     KEY           IFEQ      F8
054700000000     C                   MOVEL     '08'          FUNKEY
054800000000     C                   END
054900000000     C     KEY           IFEQ      F9
055000000000     C                   MOVEL     '09'          FUNKEY
055100000000     C                   END
055200000000     C     KEY           IFEQ      F10
055300000000     C                   MOVEL     '10'          FUNKEY
055400000000     C                   END
055500000000     C     KEY           IFEQ      F11
055600000000     C                   MOVEL     '11'          FUNKEY
055700000000     C                   END
055800000000     C     KEY           IFEQ      F12
055900000000     C                   MOVEL     '12'          FUNKEY
056000000000     C                   END
056100000000     C     KEY           IFEQ      F13
056200000000     C                   MOVEL     '13'          FUNKEY
056300000000     C                   END
056400000000     C     KEY           IFEQ      F14
056500000000     C                   MOVEL     '14'          FUNKEY
056600000000     C                   END
056700000000     C     KEY           IFEQ      F15
056800000000     C                   MOVEL     '15'          FUNKEY
056900000000     C                   END
057000000000     C     KEY           IFEQ      F16
057100000000     C                   MOVEL     '16'          FUNKEY
057200000000     C                   END
057300000000     C     KEY           IFEQ      F17
057400000000     C                   MOVEL     '17'          FUNKEY
057500000000     C                   END
057600000000     C     KEY           IFEQ      F18
057700000000     C                   MOVEL     '18'          FUNKEY
057800000000     C                   END
057900000000     C     KEY           IFEQ      F19
058000000000     C                   MOVEL     '19'          FUNKEY
058100000000     C                   END
058200000000     C     KEY           IFEQ      F20
058300000000     C                   MOVEL     '20'          FUNKEY
058400000000     C                   END
058500000000     C     KEY           IFEQ      F21
058600000000     C                   MOVEL     '21'          FUNKEY
058700000000     C                   END
058800000000     C     KEY           IFEQ      F22
058900000000     C                   MOVEL     '22'          FUNKEY
059000000000     C                   END
059100000000     C     KEY           IFEQ      F23
059200000000     C                   MOVEL     '23'          FUNKEY
059300000000     C                   END
059400000000     C     KEY           IFEQ      F24
059500000000     C                   MOVEL     '24'          FUNKEY
059600000000     C                   END
059700000000     C     KEY           IFEQ      ROLLUP
059800000000     C                   MOVEL     'RU'          FUNKEY
059900000000     C                   END
060000000000     C     KEY           IFEQ      HELP
060100000000     C                   MOVEL     'HP'          FUNKEY
060200000000     C                   END
060300000000     C                   ENDSR
060400000000     C*================================================================
060500000000     C* NOTE PER COMPILAZIONE SCHIERE
060600000000     C*================================================================
060700000000     C* -MSG-
060800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060900000000     C* (NON TOCCARE MSG,1)
061000000000     C*
061100000000     C* -CMD-
061200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061300000000     C*
061400000000     C* -FUD-
061500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
061600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
061900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
062100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
062200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
062400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
062500000000     C*
062600000000     C*================================================================
062700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062800970703TISI19R- Tasto funzionale non gestito                                          1
062900970703TISI19R- Data errata.                                                          2
063000970703TISI19R- 3                                                                     3
063100970703TISI19R- 4                                                                     4
063200970703TISI19R- 5                                                                     5
063300970703TISI19R- 6                                                                     6
063400970703TISI19R-                                                                       7
063500970703TISI19R-                                                                       8
063600970703TISI19R-                                                                       9
063700970703TISI19R-                                                                      10
063800970703TISI19R-                                                                      11
063900970703TISI19R-                                                                      12
064000970703TISI19R-                                                                      13
064100970703TISI19R-                                                                      14
064200970703TISI19R-                                                                      15
064300970703TISI19R-                                                                      16
064400970703TISI19R-                                                                      17
064500970703TISI19R-                                                                      18
064600970703TISI19R-                                                                      19
064700970703TISI19R-                                                                      20
064800000000** ======== SCHIERA: CMD  (COMANDI)                        ================
064900000000WRKSPLF
065000000000
065100000000
065200000000
065300000000
065400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
065500930201                   -        (S/N)=N                        funz.01¬
065600000000                   -        (S/N)=N                        funz.02¬
065700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
065800000000                   -        (S/N)=N                        funz.04¬
065900000000                   -        (S/N)=N                        funz.05¬
066000930208F06=Lancio         -        (S/N)=S                        funz.06¬
066100930201                   -        (S/N)=N                        funz.07¬
066200930208                   -        (S/N)=N                        funz.08¬
066300000000                   -        (S/N)=N                        funz.09¬
066400000000                   -        (S/N)=N                        funz.10¬
066500000000                   -        (S/N)=N                        funz.11¬
066600930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
066700000000                   -        (S/N)=N                        funz.13¬
066800000000                   -        (S/N)=N                        funz.14¬
066900000000                   -        (S/N)=N                        funz.15¬
067000000000                   -        (S/N)=N                        funz.16¬
067100000000                   -        (S/N)=N                        funz.17¬
067200000000                   -        (S/N)=N                        funz.18¬
067300000000                   -        (S/N)=N                        funz.19¬
067400000000                   -        (S/N)=N                        funz.20¬
067500930208                   -        (S/N)=N                        funz.21¬
067600930225F22=Gestione stampe-        (S/N)=N                        funz.22¬
067700000000                   -        (S/N)=N                        funz.23¬
067800000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

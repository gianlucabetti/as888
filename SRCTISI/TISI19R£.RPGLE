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
006200941109     D*----------------
006300970910     D* DS "XFIL" - REPERIMENTO DATI UTENTE
006400941109     D*----------------
006500930204     D DSFIL         E DS
006600980206     D*------------
006700980206     D* DS DI PROCEDURA
006800980206     D*------------
006900980206     D DSSI00        E DS                  EXTNAME(TISI00DS)
007000980206     D*----------------
007100980206     D* DS PARAMETRI DI LANCIO
007200980206     D*----------------
007300980206     D PARAM           DS                  INZ
007400980206     D  PARDAT                 1      8  0
007500941109     D*----------------
007600930201     D* ARCHITETTURA
007700941109     D*----------------
007800930201     D KPJBA         E DS
007900941109     C*------------------------------------------------------------------------*
008000941109     C* MAIN LINE
008100941109     C*------------------------------------------------------------------------*
008200000000     C*
008300000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008400000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
008500000000     C*
008600000000     C* CICLO FINO A CHE:
008700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008900930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009000000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009100000000     C     F12L00        OREQ      '1'
009200930202     C     FLFINE        OREQ      '1'
009300000000     C*
009400000000     C* EMETTE FORMATO VIDEO
009500000000     C                   EXSR      BUTFMR
009600000000     C*
009700000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
009800000000     C                   EXSR      FUNCON
009900000000     C*
010000000000     C                   END                                                    <-- 2 --
010100000000     C*
010200980206     C                   ELSE
010300980206     C                   MOVEL     '1'           D00ERR
010400980206     C                   MOVEL     DSMSMS        D00MSG
010500000000     C                   END                                                    <-- 1 --
010600000000     C*
010700000000     C                   SETON                                        LR
010800000000     C*================================================================
010900000000     C* EMISSIONE FORMATO A VIDEO
011000000000     C*================================================================
011100000000     C     BUTFMR        BEGSR
011200930204     C*
011300930204     C* WRITE RECORD TESTATA
011400970703     C                   WRITE     SI19D01
011500930205     C*
011600930205     C* VISUALIZZAZIONE MESSAGGIO
011700930205     C                   EXSR      SNDMSG
011800930204     C*
011900930204     C* WRITE RECORD FUNZIONI
012000970703     C                   WRITE     SI19D02
012100000000     C*
012200930204     C* EXFMT RECORD DETTAGLIO
012300970703     C                   EXFMT     SI19D03
012400000000     C*
012500000000     C* AZZERO FLAG DI ERRORE LIVELLO
012600000000     C                   MOVEL     '0'           ERRL00
012700000000     C*
012800000000     C* AZZERO FLAG DI ERRORE
012900950824     C                   SETOFF                                       82
013000000000     C*
013100000000     C                   ENDSR
013200000000     C*================================================================
013300000000     C* SNDMSG - MANDA MESSAGGIO
013400000000     C*================================================================
013500000000     C     SNDMSG        BEGSR
013600000000     C*
013700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
013800000000     C                   MOVEL     '1'           *IN40
013900970703     C                   WRITE     SI19DMC
014000000000     C                   MOVEL     '0'           *IN40
014100000000     C*
014200000000     C* VISUALIZZAZIONE MESSAGGIO
014300000000     C                   Z-ADD     0             MSGULT            4 0
014400930202     C                   CALL      'XMSG'        XMSGPA
014500930201     C     DSMSER        IFEQ      0
014600930201     C                   MOVEL     DSMSKE        MSGKEY
014700000000     C                   END
014800000000     C                   Z-ADD     1             MSGULT
014900970703     C                   WRITE     SI19DMS                                      SFL MESS
015000970703     C                   WRITE     SI19DMC                                      CONTR MESS
015100000000     C*
015200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015300930201     C                   MOVEL     *BLANKS       DSMSMS
015400000000     C*
015500000000     C                   ENDSR
015600000000     C*================================================================
015700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
015800000000     C*================================================================
015900000000     C     ENTGES        BEGSR
016000930201     C*
016100930201     C* CONTROLLA I CAMPI IMMESSI
016200930201     C                   EXSR      DECODI
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*================================================================
016600930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
016700000000     C*================================================================
016800000000     C     F01GES        BEGSR
016900000000     C                   ENDSR
017000000000     C*================================================================
017100930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017200000000     C*================================================================
017300000000     C     F03GES        BEGSR
017400000000     C*
017500930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017600000000     C                   MOVEL     '1'           F03L00
017700980206     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
017800000000     C*
017900000000     C                   ENDSR
018000930201     C*================================================================
018100930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018200930201     C*================================================================
018300930201     C     F06GES        BEGSR
018400960110     C*
018500960110     C* CONTROLLA I CAMPI IMMESSI
018600960110     C                   EXSR      DECODI
018700930201     C*
018800930208     C* LANCIO IL PROGRAMMA DI STAMPA
018900960110IF  1C     ERRL00        IFEQ      '0'
019000930208     C                   EXSR      VIDPAR
019100970910     C                   MOVEL     PARAM         KPJBU
019200970910     C                   MOVEL     'SI20'        KCOAZ
019300020603     C**                 CALL      'BCH10'
019400020603     C                   CALL      'TISI20R'
019500930208     C                   PARM                    KPJBA
019600970910     C                   MOVEL     '1'           FLFINE
019700960110E   1C                   ENDIF
019800930201     C*
019900930201     C                   ENDSR
020000000000     C*================================================================
020100930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
020200000000     C*================================================================
020300000000     C     F07GES        BEGSR
020400000000     C                   ENDSR
020500000000     C*================================================================
020600930208     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
020700000000     C*================================================================
020800000000     C     F08GES        BEGSR
020900000000     C                   ENDSR
021000000000     C*================================================================
021100930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
021200000000     C*================================================================
021300000000     C     F12GES        BEGSR
021400000000     C*
021500000000     C* IMPOSTO FLAG DI LIVELLO
021600000000     C                   MOVEL     '1'           F12L00
021700980206     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021800000000     C*
021900000000     C                   ENDSR
022000930201     C*================================================================
022100930208     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
022200930201     C*================================================================
022300930201     C     F21GES        BEGSR
022400930201     C                   ENDSR
022500000000     C*================================================================
022600930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
022700000000     C*================================================================
022800000000     C     F22GES        BEGSR
022900000000     C*
023000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
023100000000     C                   Z-ADD     80            LENGH            15 5
023200000000     C                   CALL      'QCAEXEC'
023300000000     C                   PARM                    CMD(1)
023400000000     C                   PARM                    LENGH
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*================================================================
023800930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023900000000     C*================================================================
024000000000     C     F24GES        BEGSR
024100000000     C*
024200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
024300000000     C                   EXSR      FUNGES
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*================================================================
024700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
024800000000     C*================================================================
024900000000     C     HELGES        BEGSR
025000000000     C                   ENDSR
025100000000     C*================================================================
025200930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
025300000000     C*================================================================
025400000000     C     DECODI        BEGSR
025500930720     C*
025600970910     C* DATA DI RIFERIMENTO
025700970910     C                   Z-ADD     VIDDDE        G08DAT
025800970910     C                   Z-ADD     *ZEROS        G08INV
025900970910     C                   MOVEL     '0'           G08ERR
026000950824     C                   CALL      'XSRDA8'
026100970910     C                   PARM                    WLBDA8
026200970910     C     G08ERR        IFEQ      '0'
026300970910     C                   Z-ADD     G08DAT        VIDDDE
026400970910     C                   Z-ADD     G08INV        WDDEI             8 0
026500950824     C                   ELSE
026600950824     C                   MOVEL     '1'           ERRL00
026700950824     C                   MOVEL     MSG(2)        DSMSMS
026800950824     C                   SETON                                        82
026900950824     C                   ENDIF
027000000000     C*
027100000000     C                   ENDSR
027200930201     C*================================================================
027300930208     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
027400930201     C*================================================================
027500930208     C     VIDPAR        BEGSR
027600930720     C*
027700950824     C* DATA RIFERIMENTO
027800950824     C                   Z-ADD     WDDEI         PARDAT
027900930201     C*
028000930201     C                   ENDSR
028100000000     C*================================================================
028200970910     C* *INZSR - OPERAZIONI INIZIALI
028300000000     C*================================================================
028400970910     C     *INZSR        BEGSR
028500930208     C*
028600930208     C* RICEVIMENTO PARAMETRI
028700930208     C     *ENTRY        PLIST
028800930208     C                   PARM                    KPJBA
028900980206     C                   PARM                    DSSI00
029000930201     C*
029100930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
029200930201     C     XMSGPA        PLIST
029300930201     C                   PARM                    DSMSMS
029400930201     C                   PARM                    DSMSPG
029500930201     C                   PARM                    DSMSKE
029600930201     C                   PARM                    DSMSER
029700930201     C*
029800930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
029900930201     C                   CALL      'XKEY'
030000930201     C                   PARM                    DSKEY
030100930201     C*
030200930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
030300930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
030400930201     C*
030500930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
030600930201     C*   -NOME PGM CHIAMANTE
030700930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
030800970703     C                   MOVEL     'TISI19R'     DSMSPG
030900970703     C                   MOVEL     'TISI19R'     MSGQUE
031000930201     C                   MOVEL     *BLANKS       MSGKEY
031100930201     C                   MOVEL     *BLANKS       DSMSMS
031200930201     C*
031300930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
031400930201     C                   Z-ADD     0             FUNGRU            5 0
031500930201     C*
031600930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
031700930201     C                   EXSR      FUNGES
031800930201     C*
031900930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
032000930201     C                   MOVEL     '0'           F12L00            1
032100930201     C                   MOVEL     '0'           F03L00            1
032200930201     C                   MOVEL     '0'           FINL00            1
032300930201     C                   MOVEL     '0'           ERRL00            1
032400930201     C                   MOVEL     '0'           FLFINE            1
032500930204     C*
032600970910     C* REPERIMENTO DATI UTENTE
032700930204     C                   CALL      'XFIL'
032800930204     C                   PARM                    DSFIL
032900930208     C*
033000930208     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
033100970910     C                   Z-ADD     UDATE         G08DAT
033200970910     C                   Z-ADD     *ZEROS        G08INV
033300970910     C                   MOVEL     '0'           G08ERR
033400950824     C                   CALL      'XSRDA8'
033500970910     C                   PARM                    WLBDA8
033600970910     C                   Z-ADD     G08DAT        VIDDDE
033700000000     C*
033800000000     C                   ENDSR
033900000000     C*================================================================
034000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
034100000000     C*================================================================
034200000000     C     FUNGES        BEGSR
034300000000     C*
034400000000     C* PULISCO CAMPI DI LAVORO
034500000000     C                   Z-ADD     0             W                 5 0
034600000000     C*
034700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
034900000000     C     1             DO        24            I
035000000000     C                   MOVEL     FUD(I)        FUDRIG
035100000000     C*
035200000000     C* PULIZIA FUNZIONI PRECEDENTI
035300000000     C                   MOVEL     *BLANKS       FUV(I)
035400000000     C*
035500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
035600000000     C* (SALTA F24)
035700000000     C     FUDATT        IFEQ      'S'
035800000000     C     FUDFUN        ANDNE     '24'
035900000000     C                   ADD       1             W
036000000000     C                   MOVEL     FUDDES        FUV(W)
036100000000     C                   END
036200000000     C                   END                                                    DO WHILE
036300000000     C*
036400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036500000000     C* ALTRIMENTI DISATTIVO F24
036600000000     C     1             DO        24            I
036700000000     C                   MOVEL     FUD(I)        FUDRIG
036800000000     C*
036900000000     C     FUDFUN        IFEQ      '24'
037000000000     C     W             IFGT      6
037100000000     C                   MOVEL     'S'           FUDATT
037200000000     C                   ELSE
037300000000     C                   MOVEL     *BLANKS       FUDATT
037400000000     C                   END                                                    W GT 8
037500000000     C                   MOVEL     FUDRIG        FUD(I)
037600000000     C                   END
037700000000     C                   END                                                    DO WHILE
037800000000     C*
037900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
038000000000     C     W             DIV       6             I
038100000000     C                   MVR                     Z
038200000000     C*
038300000000     C     Z             IFNE      0
038400000000     C     I             ADD       1             Z
038500000000     C                   ELSE
038600000000     C                   Z-ADD     I             Z                 5 0
038700000000     C                   END
038800000000     C*
038900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
039000000000     C                   ADD       1             FUNGRU
039100000000     C     FUNGRU        IFGT      Z
039200000000     C     FUNGRU        ORLT      1
039300000000     C                   Z-ADD     1             FUNGRU
039400000000     C                   END
039500000000     C*
039600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
039700000000     C     FUNGRU        IFEQ      1
039800000000     C                   MOVEL     FUV(1)        FUN001
039900000000     C                   MOVEL     FUV(2)        FUN002
040000000000     C                   MOVEL     FUV(3)        FUN003
040100000000     C                   MOVEL     FUV(4)        FUN004
040200000000     C                   MOVEL     FUV(5)        FUN005
040300000000     C                   MOVEL     FUV(6)        FUN006
040400000000     C                   END
040500000000     C*
040600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
040700000000     C     FUNGRU        IFEQ      2
040800000000     C                   MOVEL     FUV(7)        FUN001
040900000000     C                   MOVEL     FUV(8)        FUN002
041000000000     C                   MOVEL     FUV(9)        FUN003
041100000000     C                   MOVEL     FUV(10)       FUN004
041200000000     C                   MOVEL     FUV(11)       FUN005
041300000000     C                   MOVEL     FUV(12)       FUN006
041400000000     C                   END
041500000000     C*
041600000000     C* TERZO GRUPPO DI 6 FUNZIONI
041700000000     C     FUNGRU        IFEQ      3
041800000000     C                   MOVEL     FUV(13)       FUN001
041900000000     C                   MOVEL     FUV(14)       FUN002
042000000000     C                   MOVEL     FUV(15)       FUN003
042100000000     C                   MOVEL     FUV(16)       FUN004
042200000000     C                   MOVEL     FUV(17)       FUN005
042300000000     C                   MOVEL     FUV(18)       FUN006
042400000000     C                   END
042500000000     C*
042600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
042700000000     C     FUNGRU        IFEQ      4
042800000000     C                   MOVEL     FUV(19)       FUN001
042900000000     C                   MOVEL     FUV(20)       FUN002
043000000000     C                   MOVEL     FUV(21)       FUN003
043100000000     C                   MOVEL     FUV(22)       FUN004
043200000000     C                   MOVEL     *BLANKS       FUN005
043300000000     C                   MOVEL     *BLANKS       FUN006
043400000000     C                   END
043500000000     C*
043600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
043700000000     C                   MOVEL     *BLANKS       FUN007
043800000000     C                   MOVEL     *BLANKS       FUN008
043900000000     C*
044000000000     C     1             DO        24            I
044100000000     C                   MOVEL     FUD(I)        FUDRIG
044200000000     C     FUDFUN        IFEQ      '24'
044300000000     C     FUDATT        ANDEQ     'S'
044400000000     C                   MOVEL     FUDDES        FUN008
044500000000     C                   END
044600000000     C                   END                                                    DO
044700000000     C                   ENDSR
044800000000     C*================================================================
044900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
045000000000     C*================================================================
045100000000     C     FUNCON        BEGSR
045200000000     C*
045300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
045400000000     C                   EXSR      FUNVAR
045500000000     C*
045600000000     C* PULISCO CAMPI DI LAVORO
045700000000     C                   Z-ADD     0             I                 5 0
045800000000     C                   Z-ADD     0             W                 5 0
045900000000     C*
046000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
046100000000     C     FUNKEY        IFNE      *BLANKS
046200000000     C     1             DO        24            I
046300000000     C                   MOVEL     FUD(I)        FUDRIG
046400000000     C*
046500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
046600000000     C     FUNKEY        IFEQ      FUDFUN
046700000000     C     FUDATT        ANDEQ     'S'
046800000000     C                   Z-ADD     I             W
046900000000     C                   END
047000000000     C                   END                                                    DO WHILE
047100000000     C*
047200000000     C* FUNZIONE NON ATTIVA (ERRORE)
047300000000     C     W             IFEQ      0
047400000000     C*
047500000000     C* EMISSIONE MESSAGGIO DI ERRORE
047600930201     C                   MOVEL     MSG(1)        DSMSMS
047700930201     C*
047800930201     C                   END                                                    W=0
047900000000     C                   END                                                    FUNKEY NE *BLANKS
048000000000     C*
048100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
048200000000     C     W             IFNE      0
048300000000     C     FUNKEY        OREQ      *BLANKS
048400000000     C*
048500000000     C     FUNKEY        IFEQ      *BLANKS
048600000000     C                   EXSR      ENTGES
048700000000     C                   END
048800000000     C     FUNKEY        IFEQ      '01'
048900000000     C                   EXSR      F01GES
049000000000     C                   END
049100000000     C     FUNKEY        IFEQ      '03'
049200000000     C                   EXSR      F03GES
049300000000     C                   END
049400930202     C     FUNKEY        IFEQ      '06'
049500930202     C                   EXSR      F06GES
049600930202     C                   END
049700000000     C     FUNKEY        IFEQ      '07'
049800000000     C                   EXSR      F07GES
049900000000     C                   END
050000000000     C     FUNKEY        IFEQ      '08'
050100000000     C                   EXSR      F08GES
050200000000     C                   END
050300000000     C     FUNKEY        IFEQ      '12'
050400000000     C                   EXSR      F12GES
050500000000     C                   END
050600930201     C     FUNKEY        IFEQ      '21'
050700930201     C                   EXSR      F21GES
050800930201     C                   END
050900000000     C     FUNKEY        IFEQ      '22'
051000000000     C                   EXSR      F22GES
051100000000     C                   END
051200000000     C     FUNKEY        IFEQ      '24'
051300000000     C                   EXSR      F24GES
051400000000     C                   END
051500000000     C     FUNKEY        IFEQ      'HP'
051600000000     C                   EXSR      HELGES
051700000000     C                   END
051800000000     C*
051900000000     C                   END
052000000000     C*
052100000000     C                   ENDSR
052200000000     C*================================================================
052300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
052400000000     C*================================================================
052500000000     C     FUNVAR        BEGSR
052600000000     C*
052700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
052800000000     C*
052900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
053000000000     C     KEY           IFEQ      ENTER
053100000000     C                   MOVEL     *BLANKS       FUNKEY
053200000000     C                   END
053300000000     C     KEY           IFEQ      F1
053400000000     C                   MOVEL     '01'          FUNKEY
053500000000     C                   END
053600000000     C     KEY           IFEQ      F2
053700000000     C                   MOVEL     '02'          FUNKEY
053800000000     C                   END
053900000000     C     KEY           IFEQ      F3
054000000000     C                   MOVEL     '03'          FUNKEY
054100000000     C                   END
054200000000     C     KEY           IFEQ      F4
054300000000     C                   MOVEL     '04'          FUNKEY
054400000000     C                   END
054500000000     C     KEY           IFEQ      F5
054600000000     C                   MOVEL     '05'          FUNKEY
054700000000     C                   END
054800000000     C     KEY           IFEQ      F6
054900000000     C                   MOVEL     '06'          FUNKEY
055000000000     C                   END
055100000000     C     KEY           IFEQ      F7
055200000000     C                   MOVEL     '07'          FUNKEY
055300000000     C                   END
055400000000     C     KEY           IFEQ      F8
055500000000     C                   MOVEL     '08'          FUNKEY
055600000000     C                   END
055700000000     C     KEY           IFEQ      F9
055800000000     C                   MOVEL     '09'          FUNKEY
055900000000     C                   END
056000000000     C     KEY           IFEQ      F10
056100000000     C                   MOVEL     '10'          FUNKEY
056200000000     C                   END
056300000000     C     KEY           IFEQ      F11
056400000000     C                   MOVEL     '11'          FUNKEY
056500000000     C                   END
056600000000     C     KEY           IFEQ      F12
056700000000     C                   MOVEL     '12'          FUNKEY
056800000000     C                   END
056900000000     C     KEY           IFEQ      F13
057000000000     C                   MOVEL     '13'          FUNKEY
057100000000     C                   END
057200000000     C     KEY           IFEQ      F14
057300000000     C                   MOVEL     '14'          FUNKEY
057400000000     C                   END
057500000000     C     KEY           IFEQ      F15
057600000000     C                   MOVEL     '15'          FUNKEY
057700000000     C                   END
057800000000     C     KEY           IFEQ      F16
057900000000     C                   MOVEL     '16'          FUNKEY
058000000000     C                   END
058100000000     C     KEY           IFEQ      F17
058200000000     C                   MOVEL     '17'          FUNKEY
058300000000     C                   END
058400000000     C     KEY           IFEQ      F18
058500000000     C                   MOVEL     '18'          FUNKEY
058600000000     C                   END
058700000000     C     KEY           IFEQ      F19
058800000000     C                   MOVEL     '19'          FUNKEY
058900000000     C                   END
059000000000     C     KEY           IFEQ      F20
059100000000     C                   MOVEL     '20'          FUNKEY
059200000000     C                   END
059300000000     C     KEY           IFEQ      F21
059400000000     C                   MOVEL     '21'          FUNKEY
059500000000     C                   END
059600000000     C     KEY           IFEQ      F22
059700000000     C                   MOVEL     '22'          FUNKEY
059800000000     C                   END
059900000000     C     KEY           IFEQ      F23
060000000000     C                   MOVEL     '23'          FUNKEY
060100000000     C                   END
060200000000     C     KEY           IFEQ      F24
060300000000     C                   MOVEL     '24'          FUNKEY
060400000000     C                   END
060500000000     C     KEY           IFEQ      ROLLUP
060600000000     C                   MOVEL     'RU'          FUNKEY
060700000000     C                   END
060800000000     C     KEY           IFEQ      HELP
060900000000     C                   MOVEL     'HP'          FUNKEY
061000000000     C                   END
061100000000     C                   ENDSR
061200000000     C*================================================================
061300000000     C* NOTE PER COMPILAZIONE SCHIERE
061400000000     C*================================================================
061500000000     C* -MSG-
061600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
061700000000     C* (NON TOCCARE MSG,1)
061800000000     C*
061900000000     C* -CMD-
062000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
062100000000     C*
062200000000     C* -FUD-
062300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
062400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
062700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
062900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
063000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
063100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
063200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
063300000000     C*
063400000000     C*================================================================
063500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
063600970703TISI19R- Tasto funzionale non gestito                                          1
063700970703TISI19R- Data errata.                                                          2
063800970703TISI19R- 3                                                                     3
063900970703TISI19R- 4                                                                     4
064000970703TISI19R- 5                                                                     5
064100970703TISI19R- 6                                                                     6
064200970703TISI19R-                                                                       7
064300970703TISI19R-                                                                       8
064400970703TISI19R-                                                                       9
064500970703TISI19R-                                                                      10
064600970703TISI19R-                                                                      11
064700970703TISI19R-                                                                      12
064800970703TISI19R-                                                                      13
064900970703TISI19R-                                                                      14
065000970703TISI19R-                                                                      15
065100970703TISI19R-                                                                      16
065200970703TISI19R-                                                                      17
065300970703TISI19R-                                                                      18
065400970703TISI19R-                                                                      19
065500970703TISI19R-                                                                      20
065600000000** ======== SCHIERA: CMD  (COMANDI)                        ================
065700000000WRKSPLF
065800000000
065900000000
066000000000
066100000000
066200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
066300930201                   -        (S/N)=N                        funz.01¬
066400000000                   -        (S/N)=N                        funz.02¬
066500000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
066600000000                   -        (S/N)=N                        funz.04¬
066700000000                   -        (S/N)=N                        funz.05¬
066800930208F06=Lancio         -        (S/N)=S                        funz.06¬
066900930201                   -        (S/N)=N                        funz.07¬
067000930208                   -        (S/N)=N                        funz.08¬
067100000000                   -        (S/N)=N                        funz.09¬
067200000000                   -        (S/N)=N                        funz.10¬
067300000000                   -        (S/N)=N                        funz.11¬
067400930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
067500000000                   -        (S/N)=N                        funz.13¬
067600000000                   -        (S/N)=N                        funz.14¬
067700000000                   -        (S/N)=N                        funz.15¬
067800000000                   -        (S/N)=N                        funz.16¬
067900000000                   -        (S/N)=N                        funz.17¬
068000000000                   -        (S/N)=N                        funz.18¬
068100000000                   -        (S/N)=N                        funz.19¬
068200000000                   -        (S/N)=N                        funz.20¬
068300930208                   -        (S/N)=N                        funz.21¬
068400930225F22=Gestione stampe-        (S/N)=N                        funz.22¬
068500000000                   -        (S/N)=N                        funz.23¬
068600000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

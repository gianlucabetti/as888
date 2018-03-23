000100980304     H*------------------------------------------------------------------------*
000200041221     H* STATISTICA UTILIZZO SERVIZI VAS INTERNET
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800041231     FTIST35D   CF   E             WORKSTN INFDS(DEVDS1)
000900041231     F                                     SFILE(ST35DMS:MSGULT)
001000980304     D*------------------------------------------------------------------------*
001100980304     D* SCHIERE
001200980304     D*------------------------------------------------------------------------*
001300041220     D MSG             S             80    DIM(10) CTDATA PERRCD(1)
001400980304     D*------------
001500980304     D* DESCRIZIONI TASTI FUNZIONALI
001600980304     D*------------
001700980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001800980304     D*------------
001900980304     D* FUNZIONI DA CARICARE A VIDEO
002000980304     D*------------
002100980304     D FUV             S             19    DIM(24)
002200980304     D*------------------------------------------------------------------------*
002300980304     D* INPUT
002400980304     D*------------------------------------------------------------------------*
002500980304     D*------------
002600980304     D* RIDEFINIZIONE FUNZIONI VIDEO
002700980304     D*------------
002800980304     D FUDDS           DS
002900980304     D  FUDDE1                 1      1
003000980304     D  FUDFUN                 2      3
003100980304     D  FUDDE2                 4     19
003200980304     D  FUDFI0                20     34
003300980304     D  FUDATT                35     35
003400980304     D  FUDFI1                36     80
003500980304     D  FUDDES                 1     19
003600980304     D  FUDRIG                 1     80
003700980304     D*------------
003800980304     D* DS GESTIONE TASTI FUNZIONALI
003900980304     D*------------
004000980304     D DSKEY         E DS
004100980304     D DEVDS1          DS
004200980304     D  KEY                  369    369
004300980304     D*------------
004400980304     D* DS "XMSG" - GESTIONE MESSAGGI
004500980304     D*------------
004600980304     D DSMSG         E DS
004700041221     D*------------------
004800041221     D* ARCHITETTURA
004900041221     D*------------------
005000041221     D KPJBA         E DS
005100980304     D*------------------
005200041221     D* DS PASSAGGIO PARAMETRI
005300980304     D*------------------
005400041221     D PARAM           DS
005500041231     D  PAREML                        1
005600041231     D  PARCAT                        1
005700041231     D  PARCATFLG                     1
005800041231     D  PARTIP                        2
005900041231     D  PARSPED                       6  0
006000980304     C*------------------------------------------------------------------------*
006100980304     C* MAIN LINES
006200980304     C*------------------------------------------------------------------------*
006300041231     C*
006400041231     C* INIZIALIZZAZIONI INIZIALI
006500041231     C                   EXSR      AZZVID
006600980304     C*
006700980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006800041221     C     F03L00        DOUEQ     '1'
006900980304     C     F12L00        OREQ      '1'
007000980304     C     FINL00        OREQ      '1'
007100980304     C*
007200980304     C* EMETTE FORMATO VIDEO
007300980304     C                   EXSR      BUTFMR
007400980304     C*
007500980304     C* GESTIONE TASTI FUNZIONALI
007600980304     C                   EXSR      FUNCON
007700041221     C                   ENDDO
007800980304     C*
007900980304     C                   SETON                                        LR
008000980304     C*------------------------------------------------------------------------*
008100980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
008200980304     C*------------------------------------------------------------------------*
008300980304     C     BUTFMR        BEGSR
008400980304     C*
008500980304     C* EMETTE LA TESTATA
008600041231     C                   WRITE     ST35D01
008700980304     C*
008800980304     C* SPEDISCE EVENTUALE MESSAGGIO
008900980304     C                   EXSR      SNDMSG
009000980304     C*
009100980304     C* EMETTE LE FUNZIONI
009200041231     C                   WRITE     ST35D02
009300980304     C*
009400980304     C* EMETTE IL VIDEO
009500041231     C                   EXFMT     ST35D03
009600980304     C*
009700980304     C* REIMPOSTA FLAG DI ERRORE
009800980304     C                   MOVEL     '0'           ERRL00
009900050107     C*
010000050107     C* SPEGNE INDICATORI D ERRORE A VIDEO
010100050107     C                   SETOFF                                       50
010200980304     C*
010300980304     C                   ENDSR
010400980304     C*------------------------------------------------------------------------*
010500980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010600980304     C*------------------------------------------------------------------------*
010700980304     C     SNDMSG        BEGSR
010800980304     C*
010900980304     C* PULISCE IL SFILE MESSAGGI
011000980304     C                   MOVEL     '1'           *IN40
011100041231     C                   WRITE     ST35DMC
011200980304     C                   MOVEL     '0'           *IN40
011300980304     C*
011400980304     C* VISUALIZZA IL MESSAGGIO
011500980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
011600980304     C                   CALL      'XMSG'        XMSGPA
011700041220     C     DSMSER        IFEQ      *ZEROS
011800980304     C                   MOVEL     DSMSKE        MSGKEY
011900041220     C                   ENDIF
012000980304     C                   Z-ADD     1             MSGULT
012100041231     C                   WRITE     ST35DMS
012200041231     C                   WRITE     ST35DMC
012300980304     C*
012400980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
012500980304     C                   MOVEL     *BLANKS       DSMSMS
012600980304     C*
012700980304     C                   ENDSR
012800980304     C*------------------------------------------------------------------------*
012900980304     C* ENTGES - GESTIONE ENTER -> INVIO
013000980304     C*------------------------------------------------------------------------*
013100980304     C     ENTGES        BEGSR
013200980304     C*
013300050107     C                   EXSR      DECODI
013400041220     C*
013500980304     C                   ENDSR
013600980304     C*------------------------------------------------------------------------*
013700980304     C* F03GES - GESTIONE F03 -> FINE
013800980304     C*------------------------------------------------------------------------*
013900980304     C     F03GES        BEGSR
014000980304     C*
014100980304     C* IMPOSTA "PREMUTO F03"
014200980304     C                   MOVEL     '1'           F03L00
014300980304     C*
014400980304     C                   ENDSR
014500041221     C*------------------------------------------------------------------------*
014600041221     C* F06GES - GESTIONE F06 -> CONFERMA ELABORAZIONE
014700041221     C*------------------------------------------------------------------------*
014800041221     C     F06GES        BEGSR
014900050107     C*
015000050107     C* ESEGUO ROUTINE D VERIFICA DATI A VIDEO
015100050107     C                   EXSR      DECODI
015200041221     C*
015300050107     C* SE NO ERRORI => SOTTOMETTO L'ELABORAZOINE
015400050107     C                   IF        ERRL00 = '0'
015500041231     C                   MOVEL     'ST36'        KCOAZ
015600041231     C                   EVAL      PAREML    = VIDEML
015700041231     C                   EVAL      PARCAT    = VIDCAT
015800041231     C                   EVAL      PARCATFLG = VIDCATFLG
015900041231     C                   EVAL      PARTIP    = VIDTIP
016000041231     C                   EVAL      PARSPED   = VIDSPED
016100041221     C                   MOVEL     PARAM         KPJBU
016200041221     C                   CALL      'BCH10'
016300041221     C                   PARM                    KPJBA
016400041221     C*
016500041221     C* DOPO LA SOTTOMISSION ESCO DAL *PGM
016600041221     C                   MOVEL     '1'           F03L00
016700050107     C                   ENDIF
016800041221     C*
016900041221     C                   ENDSR
017000980304     C*------------------------------------------------------------------------*
017100980304     C* F12GES - GESTIONE F12 -> RITORNO
017200980304     C*------------------------------------------------------------------------*
017300980304     C     F12GES        BEGSR
017400980304     C*
017500980304     C* IMPOSTA "PREMUTO F12"
017600980304     C                   MOVEL     '1'           F12L00
017700980304     C*
017800980304     C                   ENDSR
017900980304     C*------------------------------------------------------------------------*
018000980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
018100980304     C*------------------------------------------------------------------------*
018200980304     C     F24GES        BEGSR
018300980304     C*
018400980304     C                   EXSR      FUNGES
018500980304     C*
018600980304     C                   ENDSR
018700050107     C*------------------------------------------------------------------------*
018800050107     C* DECODI - CONTROLLO DATI A VIDEO
018900050107     C*------------------------------------------------------------------------*
019000050107     C     DECODI        BEGSR
019100050107     C*
019200050107     C* RE-INIZIALIZZAZIONE FLAG ERRORE
019300050107     C                   MOVEL     '0'           ERRL00
019400050107     C*
019500050107     C* SPENGO INDICATORI D ERRORE A VIDEO
019600050107     C                   SETOFF                                       50
019700050107     C*
019800050107     C* CONTROLLO PARAMETRO INCLISIONE/ESCLUSIONE CATEGORIA SELEZIONATA
019900050107     C                   IF        VIDCAT    <> *blanks AND
020000050107     C                             VIDCATFLG =  *blanks
020100050107     C                   MOVEL     '1'           ERRL00
020200050107     C                   SETON                                        50
020300050107     C                   MOVEL     MSG(2)        DSMSMS
020400050107     C                   ENDIF
020500050107     C*
020600050107     C                   ENDSR
020700980304     C*------------------------------------------------------------------------*
020800980304     C* *INZSR - OPERAZIONI INIZIALI
020900980304     C*------------------------------------------------------------------------*
021000980304     C     *INZSR        BEGSR
021100980304     C*---
021200980304     C* RICEVIMENTO PARAMETRI
021300980304     C*---
021400980304     C     *ENTRY        PLIST
021500980304     C                   PARM                    KPJBA
021600980304     C*
021700980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
021800980304     C     XMSGPA        PLIST
021900980304     C                   PARM                    DSMSMS
022000980304     C                   PARM                    DSMSPG
022100980304     C                   PARM                    DSMSKE
022200980304     C                   PARM                    DSMSER
022300980304     C*
022400980304     C* IMPOSTA LA DS -TASTI PREMUTI-
022500980304     C                   CALL      'XKEY'
022600980304     C                   PARM                    DSKEY
022700980304     C*---
022800980304     C* VARIABILI NON RIFERITE AL DATA BASE
022900980304     C*---
023000980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
023100980304     C*---
023200980304     C* VARIABILI DI CONTROLLO
023300980304     C*---
023400980304     C                   MOVEL     '0'           FINL00            1
023500980304     C                   MOVEL     '0'           F03L00            1
023600980304     C                   MOVEL     '0'           F12L00            1
023700980304     C                   MOVEL     '0'           ERRL00            1
023800980304     C*
023900980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
024000041231     C                   MOVEL     'TIST35R'     DSMSPG
024100041231     C                   MOVEL     'TIST35R'     MSGQUE
024200980304     C                   MOVEL     *BLANKS       MSGKEY
024300980304     C                   MOVEL     *BLANKS       DSMSMS
024400980304     C*
024500980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
024600980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
024700980304     C                   EXSR      FUNGES
024800980304     C*
024900980304     C                   ENDSR
025000980304     C*------------------------------------------------------------------------*
025100980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
025200980304     C*------------------------------------------------------------------------*
025300980304     C     FUNGES        BEGSR
025400980304     C*
025500980304     C* PULISCO CAMPI DI LAVORO
025600980304     C                   Z-ADD     0             W                 5 0
025700980304     C*
025800980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
025900980304     C* E CARICO TABELLA FUNZIONI ATTIVE
026000980304     C     1             DO        24            I
026100980304     C                   MOVEL     FUD(I)        FUDRIG
026200980304     C*
026300980304     C* PULIZIA FUNZIONI PRECEDENTI
026400980304     C                   MOVEL     *BLANKS       FUV(I)
026500980304     C*
026600980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
026700980304     C* (SALTA F24)
026800980304     C     FUDATT        IFEQ      'S'
026900980304     C     FUDFUN        ANDNE     '24'
027000980304     C                   ADD       1             W
027100980304     C                   MOVEL     FUDDES        FUV(W)
027200980304     C                   END
027300980304     C                   END
027400980304     C*
027500980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
027600980304     C* ALTRIMENTI DISATTIVO F24
027700980304     C     1             DO        24            I
027800980304     C                   MOVEL     FUD(I)        FUDRIG
027900980304     C*
028000980304     C     FUDFUN        IFEQ      '24'
028100980304     C     W             IFGT      6
028200980304     C                   MOVEL     'S'           FUDATT
028300980304     C                   ELSE
028400980304     C                   MOVEL     *BLANKS       FUDATT
028500980304     C                   END
028600980304     C                   MOVEL     FUDRIG        FUD(I)
028700980304     C                   END
028800980304     C                   END
028900980304     C*
029000980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
029100980304     C     W             DIV       6             I
029200980304     C                   MVR                     Z
029300980304     C*
029400980304     C     Z             IFNE      0
029500980304     C     I             ADD       1             Z
029600980304     C                   ELSE
029700980304     C                   Z-ADD     I             Z                 5 0
029800980304     C                   END
029900980304     C*
030000980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
030100980304     C                   ADD       1             FUNGRU
030200980304     C     FUNGRU        IFGT      Z
030300980304     C     FUNGRU        ORLT      1
030400980304     C                   Z-ADD     1             FUNGRU
030500980304     C                   END
030600980304     C*
030700980304     C* PRIMO GRUPPO DI 6 FUNZIONI
030800980304     C     FUNGRU        IFEQ      1
030900980304     C                   MOVEL     FUV(1)        FUN001
031000980304     C                   MOVEL     FUV(2)        FUN002
031100980304     C                   MOVEL     FUV(3)        FUN003
031200980304     C                   MOVEL     FUV(4)        FUN004
031300980304     C                   MOVEL     FUV(5)        FUN005
031400980304     C                   MOVEL     FUV(6)        FUN006
031500980304     C                   END
031600980304     C*
031700980304     C* SECONDO GRUPPO DI 6 FUNZIONI
031800980304     C     FUNGRU        IFEQ      2
031900980304     C                   MOVEL     FUV(7)        FUN001
032000980304     C                   MOVEL     FUV(8)        FUN002
032100980304     C                   MOVEL     FUV(9)        FUN003
032200980304     C                   MOVEL     FUV(10)       FUN004
032300980304     C                   MOVEL     FUV(11)       FUN005
032400980304     C                   MOVEL     FUV(12)       FUN006
032500980304     C                   END
032600980304     C*
032700980304     C* TERZO GRUPPO DI 6 FUNZIONI
032800980304     C     FUNGRU        IFEQ      3
032900980304     C                   MOVEL     FUV(13)       FUN001
033000980304     C                   MOVEL     FUV(14)       FUN002
033100980304     C                   MOVEL     FUV(15)       FUN003
033200980304     C                   MOVEL     FUV(16)       FUN004
033300980304     C                   MOVEL     FUV(17)       FUN005
033400980304     C                   MOVEL     FUV(18)       FUN006
033500980304     C                   END
033600980304     C*
033700980304     C* QUARTO GRUPPO DI 6 FUNZIONI
033800980304     C     FUNGRU        IFEQ      4
033900980304     C                   MOVEL     FUV(19)       FUN001
034000980304     C                   MOVEL     FUV(20)       FUN002
034100980304     C                   MOVEL     FUV(21)       FUN003
034200980304     C                   MOVEL     FUV(22)       FUN004
034300980304     C                   MOVEL     *BLANKS       FUN005
034400980304     C                   MOVEL     *BLANKS       FUN006
034500980304     C                   END
034600980304     C*
034700980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
034800980304     C                   MOVEL     *BLANKS       FUN007
034900980304     C                   MOVEL     *BLANKS       FUN008
035000980304     C*
035100980304     C     1             DO        24            I
035200980304     C                   MOVEL     FUD(I)        FUDRIG
035300980304     C     FUDFUN        IFEQ      '24'
035400980304     C     FUDATT        ANDEQ     'S'
035500980304     C                   MOVEL     FUDDES        FUN008
035600980304     C                   END
035700980304     C                   END
035800980304     C*
035900980304     C                   ENDSR
036000980304     C*------------------------------------------------------------------------*
036100980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
036200980304     C*------------------------------------------------------------------------*
036300980304     C     FUNCON        BEGSR
036400980304     C*
036500980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
036600980304     C                   EXSR      FUNVAR
036700980304     C*
036800980304     C* PULISCO CAMPI DI LAVORO
036900980304     C                   Z-ADD     0             I                 5 0
037000980304     C                   Z-ADD     0             W                 5 0
037100980304     C*
037200980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
037300980304     C     FUNKEY        IFNE      *BLANKS
037400980304     C     1             DO        24            I
037500980304     C                   MOVEL     FUD(I)        FUDRIG
037600980304     C*
037700980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
037800980304     C     FUNKEY        IFEQ      FUDFUN
037900980304     C     FUDATT        ANDEQ     'S'
038000980304     C                   Z-ADD     I             W
038100980304     C                   END
038200980304     C                   END
038300980304     C*
038400980304     C* FUNZIONE NON ATTIVA (ERRORE)
038500980304     C     W             IFEQ      0
038600980304     C*
038700980304     C* EMISSIONE MESSAGGIO DI ERRORE
038800980304     C                   MOVEL     MSG(1)        DSMSMS
038900980304     C*
039000980304     C                   END
039100980304     C                   END
039200980304     C*
039300980304     C* FUNZIONE ATTIVA (PROCEDI ...)
039400980304     C     W             IFNE      0
039500980304     C     FUNKEY        OREQ      *BLANKS
039600980304     C     FUNKEY        IFEQ      *BLANKS
039700980304     C                   EXSR      ENTGES
039800980304     C                   END
039900980304     C     FUNKEY        IFEQ      '03'
040000980304     C                   EXSR      F03GES
040100980304     C                   END
040200041221     C     FUNKEY        IFEQ      '06'
040300041221     C                   EXSR      F06GES
040400041221     C                   END
040500980304     C     FUNKEY        IFEQ      '12'
040600980304     C                   EXSR      F12GES
040700980304     C                   END
040800980304     C                   END
040900980304     C*
041000980304     C                   ENDSR
041100980304     C*------------------------------------------------------------------------*
041200980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
041300980304     C*------------------------------------------------------------------------*
041400980304     C     AZZVID        BEGSR
041500041231     C*
041600050107     C                   EVAL      VIDEML    = 'N'
041700041231     C                   EVAL      VIDCAT    = 'C'
041800041231     C                   EVAL      VIDCATFLG = 'E'
041900041231     C                   EVAL      VIDTIP    = 'NA'
042000041231     C                   EVAL      VIDSPED   = 50
042100980304     C*
042200980304     C                   ENDSR
042300980304     C*------------------------------------------------------------------------*
042400980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
042500980304     C*------------------------------------------------------------------------*
042600980304     C     FUNVAR        BEGSR
042700980304     C*
042800980304     C* INIZIALIZZA VARIABILE -FUNKEY-
042900980304     C*
043000980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
043100980304     C     KEY           IFEQ      ENTER
043200980304     C                   MOVEL     *BLANKS       FUNKEY
043300980304     C                   END
043400980304     C     KEY           IFEQ      F1
043500980304     C                   MOVEL     '01'          FUNKEY
043600980304     C                   END
043700980304     C     KEY           IFEQ      F2
043800980304     C                   MOVEL     '02'          FUNKEY
043900980304     C                   END
044000980304     C     KEY           IFEQ      F3
044100980304     C                   MOVEL     '03'          FUNKEY
044200980304     C                   END
044300980304     C     KEY           IFEQ      F4
044400980304     C                   MOVEL     '04'          FUNKEY
044500980304     C                   END
044600980304     C     KEY           IFEQ      F5
044700980304     C                   MOVEL     '05'          FUNKEY
044800980304     C                   END
044900980304     C     KEY           IFEQ      F6
045000980304     C                   MOVEL     '06'          FUNKEY
045100980304     C                   END
045200980304     C     KEY           IFEQ      F7
045300980304     C                   MOVEL     '07'          FUNKEY
045400980304     C                   END
045500980304     C     KEY           IFEQ      F8
045600980304     C                   MOVEL     '08'          FUNKEY
045700980304     C                   END
045800980304     C     KEY           IFEQ      F9
045900980304     C                   MOVEL     '09'          FUNKEY
046000980304     C                   END
046100980304     C     KEY           IFEQ      F10
046200980304     C                   MOVEL     '10'          FUNKEY
046300980304     C                   END
046400980304     C     KEY           IFEQ      F11
046500980304     C                   MOVEL     '11'          FUNKEY
046600980304     C                   END
046700980304     C     KEY           IFEQ      F12
046800980304     C                   MOVEL     '12'          FUNKEY
046900980304     C                   END
047000980304     C     KEY           IFEQ      F13
047100980304     C                   MOVEL     '13'          FUNKEY
047200980304     C                   END
047300980304     C     KEY           IFEQ      F14
047400980304     C                   MOVEL     '14'          FUNKEY
047500980304     C                   END
047600980304     C     KEY           IFEQ      F15
047700980304     C                   MOVEL     '15'          FUNKEY
047800980304     C                   END
047900980304     C     KEY           IFEQ      F16
048000980304     C                   MOVEL     '16'          FUNKEY
048100980304     C                   END
048200980304     C     KEY           IFEQ      F17
048300980304     C                   MOVEL     '17'          FUNKEY
048400980304     C                   END
048500980304     C     KEY           IFEQ      F18
048600980304     C                   MOVEL     '18'          FUNKEY
048700980304     C                   END
048800980304     C     KEY           IFEQ      F19
048900980304     C                   MOVEL     '19'          FUNKEY
049000980304     C                   END
049100980304     C     KEY           IFEQ      F20
049200980304     C                   MOVEL     '20'          FUNKEY
049300980304     C                   END
049400980304     C     KEY           IFEQ      F21
049500980304     C                   MOVEL     '21'          FUNKEY
049600980304     C                   END
049700980304     C     KEY           IFEQ      F22
049800980304     C                   MOVEL     '22'          FUNKEY
049900980304     C                   END
050000980304     C     KEY           IFEQ      F23
050100980304     C                   MOVEL     '23'          FUNKEY
050200980304     C                   END
050300980304     C     KEY           IFEQ      F24
050400980304     C                   MOVEL     '24'          FUNKEY
050500980304     C                   END
050600980304     C     KEY           IFEQ      ROLLUP
050700980304     C                   MOVEL     'RU'          FUNKEY
050800980304     C                   END
050900980304     C     KEY           IFEQ      HELP
051000980304     C                   MOVEL     'HP'          FUNKEY
051100980304     C                   END
051200980304     C                   ENDSR
051300980304     C*------------------------------------------------------------------------*
051400980304     C* NOTE COSTRUZIONE SCHIERE
051500980304     C*------------------------------------------------------------------------*
051600980304     C*
051700980304     C* -MSG-MESSAGGI
051800980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
051900980304     C* (NON TOCCARE MSG,1 E MSG,2)
052000980304     C*
052100980304     C* -CMD-COMANDI
052200980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
052300980304     C*
052400980304     C* -OPD-OPZIONI
052500980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
052600980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
052700980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
052800980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
052900980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
053000980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
053100980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
053200980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
053300980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
053400980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
053500980304     C*
053600980304     C* -FUD-FUNZIONI
053700980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
053800980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
053900980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
054000980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
054100980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
054200980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
054300980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
054400980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
054500980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
054600980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
054700980304     o*-------------------------------------------------------------------------
054800980304** msg - messaggi
054900041231TIST35R- Funzione non attiva
055000050107TIST35R- Se indicata categoria cliente specificare se inclusa o esclusa
055100041231TIST35R- 3
055200041231TIST35R- 4
055300041231TIST35R- 5
055400041231TIST35R- 6
055500041231TIST35R- 7
055600041231TIST35R- 8
055700041231TIST35R- 9
055800041231TIST35R- 10
055900980304** FUD - FUNZIONI
056000980304                   -        (S/N)=N                        funz.01¬
056100980304                   -        (S/N)=N                        funz.02¬
056200980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
056300980304                   -        (S/N)=N                        funz.04¬
056400980304                   -        (S/N)=N                        funz.05¬
056500041221F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
056600980304                   -        (S/N)=N                        funz.07¬
056700980304                   -        (S/N)=N                        funz.08¬
056800980304                   -        (S/N)=N                        funz.09¬
056900980304                   -        (S/N)=N                        funz.10¬
057000980304                   -        (S/N)=N                        funz.11¬
057100980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
057200980304                   -        (S/N)=N                        funz.13¬
057300980304                   -        (S/N)=N                        funz.14¬
057400980304                   -        (S/N)=N                        funz.15¬
057500980304                   -        (S/N)=N                        funz.16¬
057600980304                   -        (S/N)=N                        funz.17¬
057700980304                   -        (S/N)=N                        funz.18¬
057800980304                   -        (S/N)=N                        funz.19¬
057900980304                   -        (S/N)=N                        funz.20¬
058000980304                   -        (S/N)=N                        funz.21¬
058100980304                   -        (S/N)=N                        funz.22¬
058200980304                   -        (S/N)=N                        funz.23¬
058300980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

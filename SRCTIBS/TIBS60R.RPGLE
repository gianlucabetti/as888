000100980304     H*------------------------------------------------------------------------*
000200980304     H* REPERIMENTO TERMINALs - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800980304     FTIBS60D   CF   E             WORKSTN INFDS(DEVDS1)
000900980304     F                                     SFILE(BS60DMS:MSGULT)
001000980304     D*------------------------------------------------------------------------*
001100980304     D* SCHIERE
001200980304     D*------------------------------------------------------------------------*
001300980304     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
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
004700980304     D*------------------
004800980304     D* DS di procedura
004900980304     D*------------------
005000980304     d lv55ds        E DS                  EXTNAME(fnlv55ds)
005100980304     D*------------------
005200980304     D* ARCHITETTURA
005300980304     D*------------------
005400980304     D KPJBA         E DS
005500980304     C*------------------------------------------------------------------------*
005600980304     C* MAIN LINES
005700980304     C*------------------------------------------------------------------------*
005800980304     C*
005900980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006000980304DO  1C     F03L00        DOUEQ     '1'
006100980304     C     F12L00        OREQ      '1'
006200980304     C     FINL00        OREQ      '1'
006300980304     C*
006400980304     C* EMETTE FORMATO VIDEO
006500980304     C                   EXSR      BUTFMR
006600980304     C*
006700980304     C* GESTIONE TASTI FUNZIONALI
006800980304     C                   EXSR      FUNCON
006900980304E   1C                   ENDDO
007000980304     C*
007100980304     C                   SETON                                        LR
007200980304     C*------------------------------------------------------------------------*
007300980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007400980304     C*------------------------------------------------------------------------*
007500980304     C     BUTFMR        BEGSR
007600980304     C*
007700980304     C* EMETTE LA TESTATA
007800980304     C                   WRITE     BS60D01
007900980304     C*
008000980304     C* SPEDISCE EVENTUALE MESSAGGIO
008100980304     C                   EXSR      SNDMSG
008200980304     C*
008300980304     C* EMETTE LE FUNZIONI
008400980304     C                   WRITE     BS60D02
008500980304     C*
008600980304     C* EMETTE IL VIDEO
008700980304     C                   EXFMT     BS60D03
008800980304     C*
008900980304     C* REIMPOSTA FLAG DI ERRORE
009000980304     C                   MOVEL     '0'           ERRL00
009100980304     C*
009200980304     C* REIMPOSTA INDICATORI DI ERRORE
009300980304     C                   SETOFF                                       59
009400980304     C                   SETOFF                                       606162
009500980304     C                   SETOFF                                       636465
009600980304     C                   SETOFF                                       666768
009700980304     C                   SETOFF                                       697071
009800980304     C                   SETOFF                                       727374
009900980304     C                   SETOFF                                       757677
010000980304     C                   SETOFF                                       787980
010100980304     C                   SETOFF                                       818283
010200980304     C                   SETOFF                                       848586
010300980304     C                   SETOFF                                       878889
010400980304     C*
010500980304     C                   ENDSR
010600980304     C*------------------------------------------------------------------------*
010700980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010800980304     C*------------------------------------------------------------------------*
010900980304     C     SNDMSG        BEGSR
011000980304     C*
011100980304     C* PULISCE IL SFILE MESSAGGI
011200980304     C                   MOVEL     '1'           *IN40
011300980304     C                   WRITE     BS60DMC
011400980304     C                   MOVEL     '0'           *IN40
011500980304     C*
011600980304     C* VISUALIZZA IL MESSAGGIO
011700980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
011800980304     C                   CALL      'XMSG'        XMSGPA
011900980304IF  1C     DSMSER        IFEQ      *ZEROS
012000980304     C                   MOVEL     DSMSKE        MSGKEY
012100980304E   1C                   ENDIF
012200980304     C                   Z-ADD     1             MSGULT
012300980304     C                   WRITE     BS60DMS
012400980304     C                   WRITE     BS60DMC
012500980304     C*
012600980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
012700980304     C                   MOVEL     *BLANKS       DSMSMS
012800980304     C*
012900980304     C                   ENDSR
013000980304     C*------------------------------------------------------------------------*
013100980304     C* ENTGES - GESTIONE ENTER -> INVIO
013200980304     C*------------------------------------------------------------------------*
013300980304     C     ENTGES        BEGSR
013400980304     C*
013500980304     c                   CALL      'FNLV55R'
013600980304     c                   PARM                    lv55ds
013700980304     C*
013800980304     C                   ENDSR
013900980304     C*------------------------------------------------------------------------*
014000980304     C* F03GES - GESTIONE F03 -> FINE
014100980304     C*------------------------------------------------------------------------*
014200980304     C     F03GES        BEGSR
014300980304     C*
014400980304     C* IMPOSTA "PREMUTO F03"
014500980304     C                   MOVEL     '1'           F03L00
014600980304     C*
014700980304     C                   ENDSR
014800980304     C*------------------------------------------------------------------------*
014900980304     C* F12GES - GESTIONE F12 -> RITORNO
015000980304     C*------------------------------------------------------------------------*
015100980304     C     F12GES        BEGSR
015200980304     C*
015300980304     C* IMPOSTA "PREMUTO F12"
015400980304     C                   MOVEL     '1'           F12L00
015500980304     C*
015600980304     C                   ENDSR
015700980304     C*------------------------------------------------------------------------*
015800980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
015900980304     C*------------------------------------------------------------------------*
016000980304     C     F24GES        BEGSR
016100980304     C*
016200980304     C                   EXSR      FUNGES
016300980304     C*
016400980304     C                   ENDSR
016500980304     C*------------------------------------------------------------------------*
016600980304     C* *INZSR - OPERAZIONI INIZIALI
016700980304     C*------------------------------------------------------------------------*
016800980304     C     *INZSR        BEGSR
016900980304     C*---
017000980304     C* RICEVIMENTO PARAMETRI
017100980304     C*---
017200980304     C     *ENTRY        PLIST
017300980304     C                   PARM                    KPJBA
017400980304     C*
017500980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
017600980304     C     XMSGPA        PLIST
017700980304     C                   PARM                    DSMSMS
017800980304     C                   PARM                    DSMSPG
017900980304     C                   PARM                    DSMSKE
018000980304     C                   PARM                    DSMSER
018100980304     C*
018200980304     C* IMPOSTA LA DS -TASTI PREMUTI-
018300980304     C                   CALL      'XKEY'
018400980304     C                   PARM                    DSKEY
018500980304     C*---
018600980304     C* VARIABILI NON RIFERITE AL DATA BASE
018700980304     C*---
018800980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
018900980304     C*---
019000980304     C* VARIABILI DI CONTROLLO
019100980304     C*---
019200980304     C                   MOVEL     '0'           FINL00            1
019300980304     C                   MOVEL     '0'           F03L00            1
019400980304     C                   MOVEL     '0'           F12L00            1
019500980304     C                   MOVEL     '0'           ERRL00            1
019600980304     C*
019700980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
019800980304     C                   MOVEL     'TIBS60R'     DSMSPG
019900980304     C                   MOVEL     'TIBS60R'     MSGQUE
020000980304     C                   MOVEL     *BLANKS       MSGKEY
020100980304     C                   MOVEL     *BLANKS       DSMSMS
020200980304     C*
020300980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
020400980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
020500980304     C                   EXSR      FUNGES
020600980304     C*
020700980304     C* IMPOSTA IL VIDEO
020800980304     c                   CLEAR                   lv55ds
020900980304     C*
021000980304     C                   ENDSR
021100980304     C*------------------------------------------------------------------------*
021200980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
021300980304     C*------------------------------------------------------------------------*
021400980304     C     FUNGES        BEGSR
021500980304     C*
021600980304     C* PULISCO CAMPI DI LAVORO
021700980304     C                   Z-ADD     0             W                 5 0
021800980304     C*
021900980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
022000980304     C* E CARICO TABELLA FUNZIONI ATTIVE
022100980304     C     1             DO        24            I
022200980304     C                   MOVEL     FUD(I)        FUDRIG
022300980304     C*
022400980304     C* PULIZIA FUNZIONI PRECEDENTI
022500980304     C                   MOVEL     *BLANKS       FUV(I)
022600980304     C*
022700980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
022800980304     C* (SALTA F24)
022900980304     C     FUDATT        IFEQ      'S'
023000980304     C     FUDFUN        ANDNE     '24'
023100980304     C                   ADD       1             W
023200980304     C                   MOVEL     FUDDES        FUV(W)
023300980304     C                   END
023400980304     C                   END
023500980304     C*
023600980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
023700980304     C* ALTRIMENTI DISATTIVO F24
023800980304     C     1             DO        24            I
023900980304     C                   MOVEL     FUD(I)        FUDRIG
024000980304     C*
024100980304     C     FUDFUN        IFEQ      '24'
024200980304     C     W             IFGT      6
024300980304     C                   MOVEL     'S'           FUDATT
024400980304     C                   ELSE
024500980304     C                   MOVEL     *BLANKS       FUDATT
024600980304     C                   END
024700980304     C                   MOVEL     FUDRIG        FUD(I)
024800980304     C                   END
024900980304     C                   END
025000980304     C*
025100980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
025200980304     C     W             DIV       6             I
025300980304     C                   MVR                     Z
025400980304     C*
025500980304     C     Z             IFNE      0
025600980304     C     I             ADD       1             Z
025700980304     C                   ELSE
025800980304     C                   Z-ADD     I             Z                 5 0
025900980304     C                   END
026000980304     C*
026100980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
026200980304     C                   ADD       1             FUNGRU
026300980304     C     FUNGRU        IFGT      Z
026400980304     C     FUNGRU        ORLT      1
026500980304     C                   Z-ADD     1             FUNGRU
026600980304     C                   END
026700980304     C*
026800980304     C* PRIMO GRUPPO DI 6 FUNZIONI
026900980304     C     FUNGRU        IFEQ      1
027000980304     C                   MOVEL     FUV(1)        FUN001
027100980304     C                   MOVEL     FUV(2)        FUN002
027200980304     C                   MOVEL     FUV(3)        FUN003
027300980304     C                   MOVEL     FUV(4)        FUN004
027400980304     C                   MOVEL     FUV(5)        FUN005
027500980304     C                   MOVEL     FUV(6)        FUN006
027600980304     C                   END
027700980304     C*
027800980304     C* SECONDO GRUPPO DI 6 FUNZIONI
027900980304     C     FUNGRU        IFEQ      2
028000980304     C                   MOVEL     FUV(7)        FUN001
028100980304     C                   MOVEL     FUV(8)        FUN002
028200980304     C                   MOVEL     FUV(9)        FUN003
028300980304     C                   MOVEL     FUV(10)       FUN004
028400980304     C                   MOVEL     FUV(11)       FUN005
028500980304     C                   MOVEL     FUV(12)       FUN006
028600980304     C                   END
028700980304     C*
028800980304     C* TERZO GRUPPO DI 6 FUNZIONI
028900980304     C     FUNGRU        IFEQ      3
029000980304     C                   MOVEL     FUV(13)       FUN001
029100980304     C                   MOVEL     FUV(14)       FUN002
029200980304     C                   MOVEL     FUV(15)       FUN003
029300980304     C                   MOVEL     FUV(16)       FUN004
029400980304     C                   MOVEL     FUV(17)       FUN005
029500980304     C                   MOVEL     FUV(18)       FUN006
029600980304     C                   END
029700980304     C*
029800980304     C* QUARTO GRUPPO DI 6 FUNZIONI
029900980304     C     FUNGRU        IFEQ      4
030000980304     C                   MOVEL     FUV(19)       FUN001
030100980304     C                   MOVEL     FUV(20)       FUN002
030200980304     C                   MOVEL     FUV(21)       FUN003
030300980304     C                   MOVEL     FUV(22)       FUN004
030400980304     C                   MOVEL     *BLANKS       FUN005
030500980304     C                   MOVEL     *BLANKS       FUN006
030600980304     C                   END
030700980304     C*
030800980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
030900980304     C                   MOVEL     *BLANKS       FUN007
031000980304     C                   MOVEL     *BLANKS       FUN008
031100980304     C*
031200980304     C     1             DO        24            I
031300980304     C                   MOVEL     FUD(I)        FUDRIG
031400980304     C     FUDFUN        IFEQ      '24'
031500980304     C     FUDATT        ANDEQ     'S'
031600980304     C                   MOVEL     FUDDES        FUN008
031700980304     C                   END
031800980304     C                   END
031900980304     C*
032000980304     C                   ENDSR
032100980304     C*------------------------------------------------------------------------*
032200980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
032300980304     C*------------------------------------------------------------------------*
032400980304     C     FUNCON        BEGSR
032500980304     C*
032600980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
032700980304     C                   EXSR      FUNVAR
032800980304     C*
032900980304     C* PULISCO CAMPI DI LAVORO
033000980304     C                   Z-ADD     0             I                 5 0
033100980304     C                   Z-ADD     0             W                 5 0
033200980304     C*
033300980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
033400980304     C     FUNKEY        IFNE      *BLANKS
033500980304     C     1             DO        24            I
033600980304     C                   MOVEL     FUD(I)        FUDRIG
033700980304     C*
033800980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
033900980304     C     FUNKEY        IFEQ      FUDFUN
034000980304     C     FUDATT        ANDEQ     'S'
034100980304     C                   Z-ADD     I             W
034200980304     C                   END
034300980304     C                   END
034400980304     C*
034500980304     C* FUNZIONE NON ATTIVA (ERRORE)
034600980304     C     W             IFEQ      0
034700980304     C*
034800980304     C* EMISSIONE MESSAGGIO DI ERRORE
034900980304     C                   MOVEL     MSG(1)        DSMSMS
035000980304     C*
035100980304     C                   END
035200980304     C                   END
035300980304     C*
035400980304     C* FUNZIONE ATTIVA (PROCEDI ...)
035500980304     C     W             IFNE      0
035600980304     C     FUNKEY        OREQ      *BLANKS
035700980304     C     FUNKEY        IFEQ      *BLANKS
035800980304     C                   EXSR      ENTGES
035900980304     C                   END
036000980304     C     FUNKEY        IFEQ      '03'
036100980304     C                   EXSR      F03GES
036200980304     C                   END
036300980304     C     FUNKEY        IFEQ      '12'
036400980304     C                   EXSR      F12GES
036500980304     C                   END
036600980304     C                   END
036700980304     C*
036800980304     C                   ENDSR
036900980304     C*------------------------------------------------------------------------*
037000980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
037100980304     C*------------------------------------------------------------------------*
037200980304     C     AZZVID        BEGSR
037300980304     C*
037400980304     C                   ENDSR
037500980304     C*------------------------------------------------------------------------*
037600980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
037700980304     C*------------------------------------------------------------------------*
037800980304     C     FUNVAR        BEGSR
037900980304     C*
038000980304     C* INIZIALIZZA VARIABILE -FUNKEY-
038100980304     C*
038200980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
038300980304     C     KEY           IFEQ      ENTER
038400980304     C                   MOVEL     *BLANKS       FUNKEY
038500980304     C                   END
038600980304     C     KEY           IFEQ      F1
038700980304     C                   MOVEL     '01'          FUNKEY
038800980304     C                   END
038900980304     C     KEY           IFEQ      F2
039000980304     C                   MOVEL     '02'          FUNKEY
039100980304     C                   END
039200980304     C     KEY           IFEQ      F3
039300980304     C                   MOVEL     '03'          FUNKEY
039400980304     C                   END
039500980304     C     KEY           IFEQ      F4
039600980304     C                   MOVEL     '04'          FUNKEY
039700980304     C                   END
039800980304     C     KEY           IFEQ      F5
039900980304     C                   MOVEL     '05'          FUNKEY
040000980304     C                   END
040100980304     C     KEY           IFEQ      F6
040200980304     C                   MOVEL     '06'          FUNKEY
040300980304     C                   END
040400980304     C     KEY           IFEQ      F7
040500980304     C                   MOVEL     '07'          FUNKEY
040600980304     C                   END
040700980304     C     KEY           IFEQ      F8
040800980304     C                   MOVEL     '08'          FUNKEY
040900980304     C                   END
041000980304     C     KEY           IFEQ      F9
041100980304     C                   MOVEL     '09'          FUNKEY
041200980304     C                   END
041300980304     C     KEY           IFEQ      F10
041400980304     C                   MOVEL     '10'          FUNKEY
041500980304     C                   END
041600980304     C     KEY           IFEQ      F11
041700980304     C                   MOVEL     '11'          FUNKEY
041800980304     C                   END
041900980304     C     KEY           IFEQ      F12
042000980304     C                   MOVEL     '12'          FUNKEY
042100980304     C                   END
042200980304     C     KEY           IFEQ      F13
042300980304     C                   MOVEL     '13'          FUNKEY
042400980304     C                   END
042500980304     C     KEY           IFEQ      F14
042600980304     C                   MOVEL     '14'          FUNKEY
042700980304     C                   END
042800980304     C     KEY           IFEQ      F15
042900980304     C                   MOVEL     '15'          FUNKEY
043000980304     C                   END
043100980304     C     KEY           IFEQ      F16
043200980304     C                   MOVEL     '16'          FUNKEY
043300980304     C                   END
043400980304     C     KEY           IFEQ      F17
043500980304     C                   MOVEL     '17'          FUNKEY
043600980304     C                   END
043700980304     C     KEY           IFEQ      F18
043800980304     C                   MOVEL     '18'          FUNKEY
043900980304     C                   END
044000980304     C     KEY           IFEQ      F19
044100980304     C                   MOVEL     '19'          FUNKEY
044200980304     C                   END
044300980304     C     KEY           IFEQ      F20
044400980304     C                   MOVEL     '20'          FUNKEY
044500980304     C                   END
044600980304     C     KEY           IFEQ      F21
044700980304     C                   MOVEL     '21'          FUNKEY
044800980304     C                   END
044900980304     C     KEY           IFEQ      F22
045000980304     C                   MOVEL     '22'          FUNKEY
045100980304     C                   END
045200980304     C     KEY           IFEQ      F23
045300980304     C                   MOVEL     '23'          FUNKEY
045400980304     C                   END
045500980304     C     KEY           IFEQ      F24
045600980304     C                   MOVEL     '24'          FUNKEY
045700980304     C                   END
045800980304     C     KEY           IFEQ      ROLLUP
045900980304     C                   MOVEL     'RU'          FUNKEY
046000980304     C                   END
046100980304     C     KEY           IFEQ      HELP
046200980304     C                   MOVEL     'HP'          FUNKEY
046300980304     C                   END
046400980304     C                   ENDSR
046500980304     C*------------------------------------------------------------------------*
046600980304     C* NOTE COSTRUZIONE SCHIERE
046700980304     C*------------------------------------------------------------------------*
046800980304     C*
046900980304     C* -MSG-MESSAGGI
047000980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
047100980304     C* (NON TOCCARE MSG,1 E MSG,2)
047200980304     C*
047300980304     C* -CMD-COMANDI
047400980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
047500980304     C*
047600980304     C* -OPD-OPZIONI
047700980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
047800980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
047900980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
048000980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
048100980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
048200980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
048300980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048400980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048500980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
048600980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
048700980304     C*
048800980304     C* -FUD-FUNZIONI
048900980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
049000980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
049100980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
049200980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
049300980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049400980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049500980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049600980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
049700980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
049800980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
049900980304     o*-------------------------------------------------------------------------
050000980304** msg - messaggi
050100980304TIBS60R- Funzione non attiva
050200980304TIBS60R- 2
050300980304TIBS60R- 3
050400980304TIBS60R- 4
050500980304TIBS60R- 5
050600980304TIBS60R- 6
050700980304TIBS60R- 7
050800980304TIBS60R- 8
050900980304TIBS60R- 9
051000980304TIBS60R- 10
051100980304** FUD - FUNZIONI
051200980304                   -        (S/N)=N                        funz.01¬
051300980304                   -        (S/N)=N                        funz.02¬
051400980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
051500980304                   -        (S/N)=N                        funz.04¬
051600980304                   -        (S/N)=N                        funz.05¬
051700980304                   -        (S/N)=N                        funz.06¬
051800980304                   -        (S/N)=N                        funz.07¬
051900980304                   -        (S/N)=N                        funz.08¬
052000980304                   -        (S/N)=N                        funz.09¬
052100980304                   -        (S/N)=N                        funz.10¬
052200980304                   -        (S/N)=N                        funz.11¬
052300980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
052400980304                   -        (S/N)=N                        funz.13¬
052500980304                   -        (S/N)=N                        funz.14¬
052600980304                   -        (S/N)=N                        funz.15¬
052700980304                   -        (S/N)=N                        funz.16¬
052800980304                   -        (S/N)=N                        funz.17¬
052900980304                   -        (S/N)=N                        funz.18¬
053000980304                   -        (S/N)=N                        funz.19¬
053100980304                   -        (S/N)=N                        funz.20¬
053200980304                   -        (S/N)=N                        funz.21¬
053300980304                   -        (S/N)=N                        funz.22¬
053400980304                   -        (S/N)=N                        funz.23¬
053500980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

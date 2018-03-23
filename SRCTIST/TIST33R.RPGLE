000100980304     H*------------------------------------------------------------------------*
000200041221     H* STATISTICA UTILIZZO SERVIZI VAS INTERNET
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800041221     FTIST33D   CF   E             WORKSTN INFDS(DEVDS1)
000900041221     F                                     SFILE(ST33DMS:MSGULT)
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
005500041221     D  PARDAI                        6  0
005600041221     D  PARDAF                        6  0
005700980304     C*------------------------------------------------------------------------*
005800980304     C* MAIN LINES
005900980304     C*------------------------------------------------------------------------*
006000980304     C*
006100980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006200041221     C     F03L00        DOUEQ     '1'
006300980304     C     F12L00        OREQ      '1'
006400980304     C     FINL00        OREQ      '1'
006500980304     C*
006600980304     C* EMETTE FORMATO VIDEO
006700980304     C                   EXSR      BUTFMR
006800980304     C*
006900980304     C* GESTIONE TASTI FUNZIONALI
007000980304     C                   EXSR      FUNCON
007100041221     C                   ENDDO
007200980304     C*
007300980304     C                   SETON                                        LR
007400980304     C*------------------------------------------------------------------------*
007500980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007600980304     C*------------------------------------------------------------------------*
007700980304     C     BUTFMR        BEGSR
007800980304     C*
007900980304     C* EMETTE LA TESTATA
008000041221     C                   WRITE     ST33D01
008100980304     C*
008200980304     C* SPEDISCE EVENTUALE MESSAGGIO
008300980304     C                   EXSR      SNDMSG
008400980304     C*
008500980304     C* EMETTE LE FUNZIONI
008600041221     C                   WRITE     ST33D02
008700980304     C*
008800980304     C* EMETTE IL VIDEO
008900041221     C                   EXFMT     ST33D03
009000980304     C*
009100980304     C* REIMPOSTA FLAG DI ERRORE
009200980304     C                   MOVEL     '0'           ERRL00
009300980304     C*
009400980304     C                   ENDSR
009500980304     C*------------------------------------------------------------------------*
009600980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009700980304     C*------------------------------------------------------------------------*
009800980304     C     SNDMSG        BEGSR
009900980304     C*
010000980304     C* PULISCE IL SFILE MESSAGGI
010100980304     C                   MOVEL     '1'           *IN40
010200041221     C                   WRITE     ST33DMC
010300980304     C                   MOVEL     '0'           *IN40
010400980304     C*
010500980304     C* VISUALIZZA IL MESSAGGIO
010600980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
010700980304     C                   CALL      'XMSG'        XMSGPA
010800041220     C     DSMSER        IFEQ      *ZEROS
010900980304     C                   MOVEL     DSMSKE        MSGKEY
011000041220     C                   ENDIF
011100980304     C                   Z-ADD     1             MSGULT
011200041221     C                   WRITE     ST33DMS
011300041221     C                   WRITE     ST33DMC
011400980304     C*
011500980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011600980304     C                   MOVEL     *BLANKS       DSMSMS
011700980304     C*
011800980304     C                   ENDSR
011900980304     C*------------------------------------------------------------------------*
012000980304     C* ENTGES - GESTIONE ENTER -> INVIO
012100980304     C*------------------------------------------------------------------------*
012200980304     C     ENTGES        BEGSR
012300980304     C*
012400041220     C*
012500980304     C                   ENDSR
012600980304     C*------------------------------------------------------------------------*
012700980304     C* F03GES - GESTIONE F03 -> FINE
012800980304     C*------------------------------------------------------------------------*
012900980304     C     F03GES        BEGSR
013000980304     C*
013100980304     C* IMPOSTA "PREMUTO F03"
013200980304     C                   MOVEL     '1'           F03L00
013300980304     C*
013400980304     C                   ENDSR
013500041221     C*------------------------------------------------------------------------*
013600041221     C* F06GES - GESTIONE F06 -> CONFERMA ELABORAZIONE
013700041221     C*------------------------------------------------------------------------*
013800041221     C     F06GES        BEGSR
013900041221     C*
014000041221     C* SOTTOMETTO L'ELABORAZOINE
014100041221     C                   MOVEL     'ST34'        KCOAZ
014200041221     C                   EVAL      PARDAI = VIDDAI
014300041221     C                   EVAL      PARDAF = VIDDAF
014400041221     C                   MOVEL     PARAM         KPJBU
014500140617     C                   CALL      'BCH10'
014600041221     C                   PARM                    KPJBA
014700041221     C*
014800041221     C* DOPO LA SOTTOMISSION ESCO DAL *PGM
014900041221     C                   MOVEL     '1'           F03L00
015000041221     C*
015100041221     C                   ENDSR
015200980304     C*------------------------------------------------------------------------*
015300980304     C* F12GES - GESTIONE F12 -> RITORNO
015400980304     C*------------------------------------------------------------------------*
015500980304     C     F12GES        BEGSR
015600980304     C*
015700980304     C* IMPOSTA "PREMUTO F12"
015800980304     C                   MOVEL     '1'           F12L00
015900980304     C*
016000980304     C                   ENDSR
016100980304     C*------------------------------------------------------------------------*
016200980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
016300980304     C*------------------------------------------------------------------------*
016400980304     C     F24GES        BEGSR
016500980304     C*
016600980304     C                   EXSR      FUNGES
016700980304     C*
016800980304     C                   ENDSR
016900980304     C*------------------------------------------------------------------------*
017000980304     C* *INZSR - OPERAZIONI INIZIALI
017100980304     C*------------------------------------------------------------------------*
017200980304     C     *INZSR        BEGSR
017300980304     C*---
017400980304     C* RICEVIMENTO PARAMETRI
017500980304     C*---
017600980304     C     *ENTRY        PLIST
017700980304     C                   PARM                    KPJBA
017800980304     C*
017900980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
018000980304     C     XMSGPA        PLIST
018100980304     C                   PARM                    DSMSMS
018200980304     C                   PARM                    DSMSPG
018300980304     C                   PARM                    DSMSKE
018400980304     C                   PARM                    DSMSER
018500980304     C*
018600980304     C* IMPOSTA LA DS -TASTI PREMUTI-
018700980304     C                   CALL      'XKEY'
018800980304     C                   PARM                    DSKEY
018900980304     C*---
019000980304     C* VARIABILI NON RIFERITE AL DATA BASE
019100980304     C*---
019200980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
019300980304     C*---
019400980304     C* VARIABILI DI CONTROLLO
019500980304     C*---
019600980304     C                   MOVEL     '0'           FINL00            1
019700980304     C                   MOVEL     '0'           F03L00            1
019800980304     C                   MOVEL     '0'           F12L00            1
019900980304     C                   MOVEL     '0'           ERRL00            1
020000980304     C*
020100980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
020200041221     C                   MOVEL     'TIST33R'     DSMSPG
020300041221     C                   MOVEL     'TIST33R'     MSGQUE
020400980304     C                   MOVEL     *BLANKS       MSGKEY
020500980304     C                   MOVEL     *BLANKS       DSMSMS
020600980304     C*
020700980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
020800980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
020900980304     C                   EXSR      FUNGES
021000980304     C*
021100980304     C                   ENDSR
021200980304     C*------------------------------------------------------------------------*
021300980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
021400980304     C*------------------------------------------------------------------------*
021500980304     C     FUNGES        BEGSR
021600980304     C*
021700980304     C* PULISCO CAMPI DI LAVORO
021800980304     C                   Z-ADD     0             W                 5 0
021900980304     C*
022000980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
022100980304     C* E CARICO TABELLA FUNZIONI ATTIVE
022200980304     C     1             DO        24            I
022300980304     C                   MOVEL     FUD(I)        FUDRIG
022400980304     C*
022500980304     C* PULIZIA FUNZIONI PRECEDENTI
022600980304     C                   MOVEL     *BLANKS       FUV(I)
022700980304     C*
022800980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
022900980304     C* (SALTA F24)
023000980304     C     FUDATT        IFEQ      'S'
023100980304     C     FUDFUN        ANDNE     '24'
023200980304     C                   ADD       1             W
023300980304     C                   MOVEL     FUDDES        FUV(W)
023400980304     C                   END
023500980304     C                   END
023600980304     C*
023700980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
023800980304     C* ALTRIMENTI DISATTIVO F24
023900980304     C     1             DO        24            I
024000980304     C                   MOVEL     FUD(I)        FUDRIG
024100980304     C*
024200980304     C     FUDFUN        IFEQ      '24'
024300980304     C     W             IFGT      6
024400980304     C                   MOVEL     'S'           FUDATT
024500980304     C                   ELSE
024600980304     C                   MOVEL     *BLANKS       FUDATT
024700980304     C                   END
024800980304     C                   MOVEL     FUDRIG        FUD(I)
024900980304     C                   END
025000980304     C                   END
025100980304     C*
025200980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
025300980304     C     W             DIV       6             I
025400980304     C                   MVR                     Z
025500980304     C*
025600980304     C     Z             IFNE      0
025700980304     C     I             ADD       1             Z
025800980304     C                   ELSE
025900980304     C                   Z-ADD     I             Z                 5 0
026000980304     C                   END
026100980304     C*
026200980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
026300980304     C                   ADD       1             FUNGRU
026400980304     C     FUNGRU        IFGT      Z
026500980304     C     FUNGRU        ORLT      1
026600980304     C                   Z-ADD     1             FUNGRU
026700980304     C                   END
026800980304     C*
026900980304     C* PRIMO GRUPPO DI 6 FUNZIONI
027000980304     C     FUNGRU        IFEQ      1
027100980304     C                   MOVEL     FUV(1)        FUN001
027200980304     C                   MOVEL     FUV(2)        FUN002
027300980304     C                   MOVEL     FUV(3)        FUN003
027400980304     C                   MOVEL     FUV(4)        FUN004
027500980304     C                   MOVEL     FUV(5)        FUN005
027600980304     C                   MOVEL     FUV(6)        FUN006
027700980304     C                   END
027800980304     C*
027900980304     C* SECONDO GRUPPO DI 6 FUNZIONI
028000980304     C     FUNGRU        IFEQ      2
028100980304     C                   MOVEL     FUV(7)        FUN001
028200980304     C                   MOVEL     FUV(8)        FUN002
028300980304     C                   MOVEL     FUV(9)        FUN003
028400980304     C                   MOVEL     FUV(10)       FUN004
028500980304     C                   MOVEL     FUV(11)       FUN005
028600980304     C                   MOVEL     FUV(12)       FUN006
028700980304     C                   END
028800980304     C*
028900980304     C* TERZO GRUPPO DI 6 FUNZIONI
029000980304     C     FUNGRU        IFEQ      3
029100980304     C                   MOVEL     FUV(13)       FUN001
029200980304     C                   MOVEL     FUV(14)       FUN002
029300980304     C                   MOVEL     FUV(15)       FUN003
029400980304     C                   MOVEL     FUV(16)       FUN004
029500980304     C                   MOVEL     FUV(17)       FUN005
029600980304     C                   MOVEL     FUV(18)       FUN006
029700980304     C                   END
029800980304     C*
029900980304     C* QUARTO GRUPPO DI 6 FUNZIONI
030000980304     C     FUNGRU        IFEQ      4
030100980304     C                   MOVEL     FUV(19)       FUN001
030200980304     C                   MOVEL     FUV(20)       FUN002
030300980304     C                   MOVEL     FUV(21)       FUN003
030400980304     C                   MOVEL     FUV(22)       FUN004
030500980304     C                   MOVEL     *BLANKS       FUN005
030600980304     C                   MOVEL     *BLANKS       FUN006
030700980304     C                   END
030800980304     C*
030900980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
031000980304     C                   MOVEL     *BLANKS       FUN007
031100980304     C                   MOVEL     *BLANKS       FUN008
031200980304     C*
031300980304     C     1             DO        24            I
031400980304     C                   MOVEL     FUD(I)        FUDRIG
031500980304     C     FUDFUN        IFEQ      '24'
031600980304     C     FUDATT        ANDEQ     'S'
031700980304     C                   MOVEL     FUDDES        FUN008
031800980304     C                   END
031900980304     C                   END
032000980304     C*
032100980304     C                   ENDSR
032200980304     C*------------------------------------------------------------------------*
032300980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
032400980304     C*------------------------------------------------------------------------*
032500980304     C     FUNCON        BEGSR
032600980304     C*
032700980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
032800980304     C                   EXSR      FUNVAR
032900980304     C*
033000980304     C* PULISCO CAMPI DI LAVORO
033100980304     C                   Z-ADD     0             I                 5 0
033200980304     C                   Z-ADD     0             W                 5 0
033300980304     C*
033400980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
033500980304     C     FUNKEY        IFNE      *BLANKS
033600980304     C     1             DO        24            I
033700980304     C                   MOVEL     FUD(I)        FUDRIG
033800980304     C*
033900980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
034000980304     C     FUNKEY        IFEQ      FUDFUN
034100980304     C     FUDATT        ANDEQ     'S'
034200980304     C                   Z-ADD     I             W
034300980304     C                   END
034400980304     C                   END
034500980304     C*
034600980304     C* FUNZIONE NON ATTIVA (ERRORE)
034700980304     C     W             IFEQ      0
034800980304     C*
034900980304     C* EMISSIONE MESSAGGIO DI ERRORE
035000980304     C                   MOVEL     MSG(1)        DSMSMS
035100980304     C*
035200980304     C                   END
035300980304     C                   END
035400980304     C*
035500980304     C* FUNZIONE ATTIVA (PROCEDI ...)
035600980304     C     W             IFNE      0
035700980304     C     FUNKEY        OREQ      *BLANKS
035800980304     C     FUNKEY        IFEQ      *BLANKS
035900980304     C                   EXSR      ENTGES
036000980304     C                   END
036100980304     C     FUNKEY        IFEQ      '03'
036200980304     C                   EXSR      F03GES
036300980304     C                   END
036400041221     C     FUNKEY        IFEQ      '06'
036500041221     C                   EXSR      F06GES
036600041221     C                   END
036700980304     C     FUNKEY        IFEQ      '12'
036800980304     C                   EXSR      F12GES
036900980304     C                   END
037000980304     C                   END
037100980304     C*
037200980304     C                   ENDSR
037300980304     C*------------------------------------------------------------------------*
037400980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
037500980304     C*------------------------------------------------------------------------*
037600980304     C     AZZVID        BEGSR
037700980304     C*
037800980304     C                   ENDSR
037900980304     C*------------------------------------------------------------------------*
038000980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
038100980304     C*------------------------------------------------------------------------*
038200980304     C     FUNVAR        BEGSR
038300980304     C*
038400980304     C* INIZIALIZZA VARIABILE -FUNKEY-
038500980304     C*
038600980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
038700980304     C     KEY           IFEQ      ENTER
038800980304     C                   MOVEL     *BLANKS       FUNKEY
038900980304     C                   END
039000980304     C     KEY           IFEQ      F1
039100980304     C                   MOVEL     '01'          FUNKEY
039200980304     C                   END
039300980304     C     KEY           IFEQ      F2
039400980304     C                   MOVEL     '02'          FUNKEY
039500980304     C                   END
039600980304     C     KEY           IFEQ      F3
039700980304     C                   MOVEL     '03'          FUNKEY
039800980304     C                   END
039900980304     C     KEY           IFEQ      F4
040000980304     C                   MOVEL     '04'          FUNKEY
040100980304     C                   END
040200980304     C     KEY           IFEQ      F5
040300980304     C                   MOVEL     '05'          FUNKEY
040400980304     C                   END
040500980304     C     KEY           IFEQ      F6
040600980304     C                   MOVEL     '06'          FUNKEY
040700980304     C                   END
040800980304     C     KEY           IFEQ      F7
040900980304     C                   MOVEL     '07'          FUNKEY
041000980304     C                   END
041100980304     C     KEY           IFEQ      F8
041200980304     C                   MOVEL     '08'          FUNKEY
041300980304     C                   END
041400980304     C     KEY           IFEQ      F9
041500980304     C                   MOVEL     '09'          FUNKEY
041600980304     C                   END
041700980304     C     KEY           IFEQ      F10
041800980304     C                   MOVEL     '10'          FUNKEY
041900980304     C                   END
042000980304     C     KEY           IFEQ      F11
042100980304     C                   MOVEL     '11'          FUNKEY
042200980304     C                   END
042300980304     C     KEY           IFEQ      F12
042400980304     C                   MOVEL     '12'          FUNKEY
042500980304     C                   END
042600980304     C     KEY           IFEQ      F13
042700980304     C                   MOVEL     '13'          FUNKEY
042800980304     C                   END
042900980304     C     KEY           IFEQ      F14
043000980304     C                   MOVEL     '14'          FUNKEY
043100980304     C                   END
043200980304     C     KEY           IFEQ      F15
043300980304     C                   MOVEL     '15'          FUNKEY
043400980304     C                   END
043500980304     C     KEY           IFEQ      F16
043600980304     C                   MOVEL     '16'          FUNKEY
043700980304     C                   END
043800980304     C     KEY           IFEQ      F17
043900980304     C                   MOVEL     '17'          FUNKEY
044000980304     C                   END
044100980304     C     KEY           IFEQ      F18
044200980304     C                   MOVEL     '18'          FUNKEY
044300980304     C                   END
044400980304     C     KEY           IFEQ      F19
044500980304     C                   MOVEL     '19'          FUNKEY
044600980304     C                   END
044700980304     C     KEY           IFEQ      F20
044800980304     C                   MOVEL     '20'          FUNKEY
044900980304     C                   END
045000980304     C     KEY           IFEQ      F21
045100980304     C                   MOVEL     '21'          FUNKEY
045200980304     C                   END
045300980304     C     KEY           IFEQ      F22
045400980304     C                   MOVEL     '22'          FUNKEY
045500980304     C                   END
045600980304     C     KEY           IFEQ      F23
045700980304     C                   MOVEL     '23'          FUNKEY
045800980304     C                   END
045900980304     C     KEY           IFEQ      F24
046000980304     C                   MOVEL     '24'          FUNKEY
046100980304     C                   END
046200980304     C     KEY           IFEQ      ROLLUP
046300980304     C                   MOVEL     'RU'          FUNKEY
046400980304     C                   END
046500980304     C     KEY           IFEQ      HELP
046600980304     C                   MOVEL     'HP'          FUNKEY
046700980304     C                   END
046800980304     C                   ENDSR
046900980304     C*------------------------------------------------------------------------*
047000980304     C* NOTE COSTRUZIONE SCHIERE
047100980304     C*------------------------------------------------------------------------*
047200980304     C*
047300980304     C* -MSG-MESSAGGI
047400980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
047500980304     C* (NON TOCCARE MSG,1 E MSG,2)
047600980304     C*
047700980304     C* -CMD-COMANDI
047800980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
047900980304     C*
048000980304     C* -OPD-OPZIONI
048100980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
048200980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
048300980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
048400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
048500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
048600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
048700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
049100980304     C*
049200980304     C* -FUD-FUNZIONI
049300980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
049400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
049500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
049600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
049700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
050000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
050100980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
050200980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
050300980304     o*-------------------------------------------------------------------------
050400980304** msg - messaggi
050500041221TIST33R- Funzione non attiva
050600041221TIST33R- 2
050700041221TIST33R- 3
050800041221TIST33R- 4
050900041221TIST33R- 5
051000041221TIST33R- 6
051100041221TIST33R- 7
051200041221TIST33R- 8
051300041221TIST33R- 9
051400041221TIST33R- 10
051500980304** FUD - FUNZIONI
051600980304                   -        (S/N)=N                        funz.01¬
051700980304                   -        (S/N)=N                        funz.02¬
051800980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
051900980304                   -        (S/N)=N                        funz.04¬
052000980304                   -        (S/N)=N                        funz.05¬
052100041221F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
052200980304                   -        (S/N)=N                        funz.07¬
052300980304                   -        (S/N)=N                        funz.08¬
052400980304                   -        (S/N)=N                        funz.09¬
052500980304                   -        (S/N)=N                        funz.10¬
052600980304                   -        (S/N)=N                        funz.11¬
052700980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
052800980304                   -        (S/N)=N                        funz.13¬
052900980304                   -        (S/N)=N                        funz.14¬
053000980304                   -        (S/N)=N                        funz.15¬
053100980304                   -        (S/N)=N                        funz.16¬
053200980304                   -        (S/N)=N                        funz.17¬
053300980304                   -        (S/N)=N                        funz.18¬
053400980304                   -        (S/N)=N                        funz.19¬
053500980304                   -        (S/N)=N                        funz.20¬
053600980304                   -        (S/N)=N                        funz.21¬
053700980304                   -        (S/N)=N                        funz.22¬
053800980304                   -        (S/N)=N                        funz.23¬
053900980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

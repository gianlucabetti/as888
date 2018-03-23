000100980304     H*------------------------------------------------------------------------*
000200041220     H* REPERIMENTO SALDI CLIENTE - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800041220     FTISE71D   CF   E             WORKSTN INFDS(DEVDS1)
000900041220     F                                     SFILE(SE71DMS:MSGULT)
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
004700980304     D*------------------
004800980304     D* DS di procedura
004900980304     D*------------------
005000041220     D TISE70DS      E DS
005100980304     D*------------------
005200980304     D* ARCHITETTURA
005300980304     D*------------------
005400980304     D KPJBA         E DS
005500980304     C*------------------------------------------------------------------------*
005600980304     C* MAIN LINES
005700980304     C*------------------------------------------------------------------------*
005800041220     C*
005900041220     C* INIZIALIZZO LA DS D PROCEDURA
006000041220     C                   CLEAR                   TISE70DS
006100980304     C*
006200980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006300980304DO  1C     F03L00        DOUEQ     '1'
006400980304     C     F12L00        OREQ      '1'
006500980304     C     FINL00        OREQ      '1'
006600980304     C*
006700980304     C* EMETTE FORMATO VIDEO
006800980304     C                   EXSR      BUTFMR
006900980304     C*
007000980304     C* GESTIONE TASTI FUNZIONALI
007100980304     C                   EXSR      FUNCON
007200980304E   1C                   ENDDO
007300980304     C*
007400980304     C                   SETON                                        LR
007500980304     C*------------------------------------------------------------------------*
007600980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007700980304     C*------------------------------------------------------------------------*
007800980304     C     BUTFMR        BEGSR
007900980304     C*
008000980304     C* EMETTE LA TESTATA
008100041220     C                   WRITE     SE71D01
008200980304     C*
008300980304     C* SPEDISCE EVENTUALE MESSAGGIO
008400980304     C                   EXSR      SNDMSG
008500980304     C*
008600980304     C* EMETTE LE FUNZIONI
008700041220     C                   WRITE     SE71D02
008800980304     C*
008900980304     C* EMETTE IL VIDEO
009000041220     C                   EXFMT     SE71D03
009100980304     C*
009200980304     C* REIMPOSTA FLAG DI ERRORE
009300980304     C                   MOVEL     '0'           ERRL00
009400980304     C*
009500980304     C                   ENDSR
009600980304     C*------------------------------------------------------------------------*
009700980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009800980304     C*------------------------------------------------------------------------*
009900980304     C     SNDMSG        BEGSR
010000980304     C*
010100980304     C* PULISCE IL SFILE MESSAGGI
010200980304     C                   MOVEL     '1'           *IN40
010300041220     C                   WRITE     SE71DMC
010400980304     C                   MOVEL     '0'           *IN40
010500980304     C*
010600980304     C* VISUALIZZA IL MESSAGGIO
010700980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
010800980304     C                   CALL      'XMSG'        XMSGPA
010900041220     C     DSMSER        IFEQ      *ZEROS
011000980304     C                   MOVEL     DSMSKE        MSGKEY
011100041220     C                   ENDIF
011200980304     C                   Z-ADD     1             MSGULT
011300041220     C                   WRITE     SE71DMS
011400041220     C                   WRITE     SE71DMC
011500980304     C*
011600980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011700980304     C                   MOVEL     *BLANKS       DSMSMS
011800980304     C*
011900980304     C                   ENDSR
012000980304     C*------------------------------------------------------------------------*
012100980304     C* ENTGES - GESTIONE ENTER -> INVIO
012200980304     C*------------------------------------------------------------------------*
012300980304     C     ENTGES        BEGSR
012400980304     C*
012500041220     C                   CALL      'TISE70R'
012600041220     C                   PARM                    TISE70DS
012700041220     C*
012800980304     C                   ENDSR
012900980304     C*------------------------------------------------------------------------*
013000980304     C* F03GES - GESTIONE F03 -> FINE
013100980304     C*------------------------------------------------------------------------*
013200980304     C     F03GES        BEGSR
013300980304     C*
013400980304     C* IMPOSTA "PREMUTO F03"
013500980304     C                   MOVEL     '1'           F03L00
013600980304     C*
013700980304     C                   ENDSR
013800980304     C*------------------------------------------------------------------------*
013900980304     C* F12GES - GESTIONE F12 -> RITORNO
014000980304     C*------------------------------------------------------------------------*
014100980304     C     F12GES        BEGSR
014200980304     C*
014300980304     C* IMPOSTA "PREMUTO F12"
014400980304     C                   MOVEL     '1'           F12L00
014500980304     C*
014600980304     C                   ENDSR
014700980304     C*------------------------------------------------------------------------*
014800980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
014900980304     C*------------------------------------------------------------------------*
015000980304     C     F24GES        BEGSR
015100980304     C*
015200980304     C                   EXSR      FUNGES
015300980304     C*
015400980304     C                   ENDSR
015500980304     C*------------------------------------------------------------------------*
015600980304     C* *INZSR - OPERAZIONI INIZIALI
015700980304     C*------------------------------------------------------------------------*
015800980304     C     *INZSR        BEGSR
015900980304     C*---
016000980304     C* RICEVIMENTO PARAMETRI
016100980304     C*---
016200980304     C     *ENTRY        PLIST
016300980304     C                   PARM                    KPJBA
016400980304     C*
016500980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
016600980304     C     XMSGPA        PLIST
016700980304     C                   PARM                    DSMSMS
016800980304     C                   PARM                    DSMSPG
016900980304     C                   PARM                    DSMSKE
017000980304     C                   PARM                    DSMSER
017100980304     C*
017200980304     C* IMPOSTA LA DS -TASTI PREMUTI-
017300980304     C                   CALL      'XKEY'
017400980304     C                   PARM                    DSKEY
017500980304     C*---
017600980304     C* VARIABILI NON RIFERITE AL DATA BASE
017700980304     C*---
017800980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
017900980304     C*---
018000980304     C* VARIABILI DI CONTROLLO
018100980304     C*---
018200980304     C                   MOVEL     '0'           FINL00            1
018300980304     C                   MOVEL     '0'           F03L00            1
018400980304     C                   MOVEL     '0'           F12L00            1
018500980304     C                   MOVEL     '0'           ERRL00            1
018600980304     C*
018700980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
018800041220     C                   MOVEL     'TISE71R'     DSMSPG
018900041220     C                   MOVEL     'TISE71R'     MSGQUE
019000980304     C                   MOVEL     *BLANKS       MSGKEY
019100980304     C                   MOVEL     *BLANKS       DSMSMS
019200980304     C*
019300980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
019400980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
019500980304     C                   EXSR      FUNGES
019600980304     C*
019700980304     C                   ENDSR
019800980304     C*------------------------------------------------------------------------*
019900980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
020000980304     C*------------------------------------------------------------------------*
020100980304     C     FUNGES        BEGSR
020200980304     C*
020300980304     C* PULISCO CAMPI DI LAVORO
020400980304     C                   Z-ADD     0             W                 5 0
020500980304     C*
020600980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
020700980304     C* E CARICO TABELLA FUNZIONI ATTIVE
020800980304     C     1             DO        24            I
020900980304     C                   MOVEL     FUD(I)        FUDRIG
021000980304     C*
021100980304     C* PULIZIA FUNZIONI PRECEDENTI
021200980304     C                   MOVEL     *BLANKS       FUV(I)
021300980304     C*
021400980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
021500980304     C* (SALTA F24)
021600980304     C     FUDATT        IFEQ      'S'
021700980304     C     FUDFUN        ANDNE     '24'
021800980304     C                   ADD       1             W
021900980304     C                   MOVEL     FUDDES        FUV(W)
022000980304     C                   END
022100980304     C                   END
022200980304     C*
022300980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
022400980304     C* ALTRIMENTI DISATTIVO F24
022500980304     C     1             DO        24            I
022600980304     C                   MOVEL     FUD(I)        FUDRIG
022700980304     C*
022800980304     C     FUDFUN        IFEQ      '24'
022900980304     C     W             IFGT      6
023000980304     C                   MOVEL     'S'           FUDATT
023100980304     C                   ELSE
023200980304     C                   MOVEL     *BLANKS       FUDATT
023300980304     C                   END
023400980304     C                   MOVEL     FUDRIG        FUD(I)
023500980304     C                   END
023600980304     C                   END
023700980304     C*
023800980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
023900980304     C     W             DIV       6             I
024000980304     C                   MVR                     Z
024100980304     C*
024200980304     C     Z             IFNE      0
024300980304     C     I             ADD       1             Z
024400980304     C                   ELSE
024500980304     C                   Z-ADD     I             Z                 5 0
024600980304     C                   END
024700980304     C*
024800980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
024900980304     C                   ADD       1             FUNGRU
025000980304     C     FUNGRU        IFGT      Z
025100980304     C     FUNGRU        ORLT      1
025200980304     C                   Z-ADD     1             FUNGRU
025300980304     C                   END
025400980304     C*
025500980304     C* PRIMO GRUPPO DI 6 FUNZIONI
025600980304     C     FUNGRU        IFEQ      1
025700980304     C                   MOVEL     FUV(1)        FUN001
025800980304     C                   MOVEL     FUV(2)        FUN002
025900980304     C                   MOVEL     FUV(3)        FUN003
026000980304     C                   MOVEL     FUV(4)        FUN004
026100980304     C                   MOVEL     FUV(5)        FUN005
026200980304     C                   MOVEL     FUV(6)        FUN006
026300980304     C                   END
026400980304     C*
026500980304     C* SECONDO GRUPPO DI 6 FUNZIONI
026600980304     C     FUNGRU        IFEQ      2
026700980304     C                   MOVEL     FUV(7)        FUN001
026800980304     C                   MOVEL     FUV(8)        FUN002
026900980304     C                   MOVEL     FUV(9)        FUN003
027000980304     C                   MOVEL     FUV(10)       FUN004
027100980304     C                   MOVEL     FUV(11)       FUN005
027200980304     C                   MOVEL     FUV(12)       FUN006
027300980304     C                   END
027400980304     C*
027500980304     C* TERZO GRUPPO DI 6 FUNZIONI
027600980304     C     FUNGRU        IFEQ      3
027700980304     C                   MOVEL     FUV(13)       FUN001
027800980304     C                   MOVEL     FUV(14)       FUN002
027900980304     C                   MOVEL     FUV(15)       FUN003
028000980304     C                   MOVEL     FUV(16)       FUN004
028100980304     C                   MOVEL     FUV(17)       FUN005
028200980304     C                   MOVEL     FUV(18)       FUN006
028300980304     C                   END
028400980304     C*
028500980304     C* QUARTO GRUPPO DI 6 FUNZIONI
028600980304     C     FUNGRU        IFEQ      4
028700980304     C                   MOVEL     FUV(19)       FUN001
028800980304     C                   MOVEL     FUV(20)       FUN002
028900980304     C                   MOVEL     FUV(21)       FUN003
029000980304     C                   MOVEL     FUV(22)       FUN004
029100980304     C                   MOVEL     *BLANKS       FUN005
029200980304     C                   MOVEL     *BLANKS       FUN006
029300980304     C                   END
029400980304     C*
029500980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
029600980304     C                   MOVEL     *BLANKS       FUN007
029700980304     C                   MOVEL     *BLANKS       FUN008
029800980304     C*
029900980304     C     1             DO        24            I
030000980304     C                   MOVEL     FUD(I)        FUDRIG
030100980304     C     FUDFUN        IFEQ      '24'
030200980304     C     FUDATT        ANDEQ     'S'
030300980304     C                   MOVEL     FUDDES        FUN008
030400980304     C                   END
030500980304     C                   END
030600980304     C*
030700980304     C                   ENDSR
030800980304     C*------------------------------------------------------------------------*
030900980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
031000980304     C*------------------------------------------------------------------------*
031100980304     C     FUNCON        BEGSR
031200980304     C*
031300980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
031400980304     C                   EXSR      FUNVAR
031500980304     C*
031600980304     C* PULISCO CAMPI DI LAVORO
031700980304     C                   Z-ADD     0             I                 5 0
031800980304     C                   Z-ADD     0             W                 5 0
031900980304     C*
032000980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
032100980304     C     FUNKEY        IFNE      *BLANKS
032200980304     C     1             DO        24            I
032300980304     C                   MOVEL     FUD(I)        FUDRIG
032400980304     C*
032500980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
032600980304     C     FUNKEY        IFEQ      FUDFUN
032700980304     C     FUDATT        ANDEQ     'S'
032800980304     C                   Z-ADD     I             W
032900980304     C                   END
033000980304     C                   END
033100980304     C*
033200980304     C* FUNZIONE NON ATTIVA (ERRORE)
033300980304     C     W             IFEQ      0
033400980304     C*
033500980304     C* EMISSIONE MESSAGGIO DI ERRORE
033600980304     C                   MOVEL     MSG(1)        DSMSMS
033700980304     C*
033800980304     C                   END
033900980304     C                   END
034000980304     C*
034100980304     C* FUNZIONE ATTIVA (PROCEDI ...)
034200980304     C     W             IFNE      0
034300980304     C     FUNKEY        OREQ      *BLANKS
034400980304     C     FUNKEY        IFEQ      *BLANKS
034500980304     C                   EXSR      ENTGES
034600980304     C                   END
034700980304     C     FUNKEY        IFEQ      '03'
034800980304     C                   EXSR      F03GES
034900980304     C                   END
035000980304     C     FUNKEY        IFEQ      '12'
035100980304     C                   EXSR      F12GES
035200980304     C                   END
035300980304     C                   END
035400980304     C*
035500980304     C                   ENDSR
035600980304     C*------------------------------------------------------------------------*
035700980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
035800980304     C*------------------------------------------------------------------------*
035900980304     C     AZZVID        BEGSR
036000980304     C*
036100980304     C                   ENDSR
036200980304     C*------------------------------------------------------------------------*
036300980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
036400980304     C*------------------------------------------------------------------------*
036500980304     C     FUNVAR        BEGSR
036600980304     C*
036700980304     C* INIZIALIZZA VARIABILE -FUNKEY-
036800980304     C*
036900980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
037000980304     C     KEY           IFEQ      ENTER
037100980304     C                   MOVEL     *BLANKS       FUNKEY
037200980304     C                   END
037300980304     C     KEY           IFEQ      F1
037400980304     C                   MOVEL     '01'          FUNKEY
037500980304     C                   END
037600980304     C     KEY           IFEQ      F2
037700980304     C                   MOVEL     '02'          FUNKEY
037800980304     C                   END
037900980304     C     KEY           IFEQ      F3
038000980304     C                   MOVEL     '03'          FUNKEY
038100980304     C                   END
038200980304     C     KEY           IFEQ      F4
038300980304     C                   MOVEL     '04'          FUNKEY
038400980304     C                   END
038500980304     C     KEY           IFEQ      F5
038600980304     C                   MOVEL     '05'          FUNKEY
038700980304     C                   END
038800980304     C     KEY           IFEQ      F6
038900980304     C                   MOVEL     '06'          FUNKEY
039000980304     C                   END
039100980304     C     KEY           IFEQ      F7
039200980304     C                   MOVEL     '07'          FUNKEY
039300980304     C                   END
039400980304     C     KEY           IFEQ      F8
039500980304     C                   MOVEL     '08'          FUNKEY
039600980304     C                   END
039700980304     C     KEY           IFEQ      F9
039800980304     C                   MOVEL     '09'          FUNKEY
039900980304     C                   END
040000980304     C     KEY           IFEQ      F10
040100980304     C                   MOVEL     '10'          FUNKEY
040200980304     C                   END
040300980304     C     KEY           IFEQ      F11
040400980304     C                   MOVEL     '11'          FUNKEY
040500980304     C                   END
040600980304     C     KEY           IFEQ      F12
040700980304     C                   MOVEL     '12'          FUNKEY
040800980304     C                   END
040900980304     C     KEY           IFEQ      F13
041000980304     C                   MOVEL     '13'          FUNKEY
041100980304     C                   END
041200980304     C     KEY           IFEQ      F14
041300980304     C                   MOVEL     '14'          FUNKEY
041400980304     C                   END
041500980304     C     KEY           IFEQ      F15
041600980304     C                   MOVEL     '15'          FUNKEY
041700980304     C                   END
041800980304     C     KEY           IFEQ      F16
041900980304     C                   MOVEL     '16'          FUNKEY
042000980304     C                   END
042100980304     C     KEY           IFEQ      F17
042200980304     C                   MOVEL     '17'          FUNKEY
042300980304     C                   END
042400980304     C     KEY           IFEQ      F18
042500980304     C                   MOVEL     '18'          FUNKEY
042600980304     C                   END
042700980304     C     KEY           IFEQ      F19
042800980304     C                   MOVEL     '19'          FUNKEY
042900980304     C                   END
043000980304     C     KEY           IFEQ      F20
043100980304     C                   MOVEL     '20'          FUNKEY
043200980304     C                   END
043300980304     C     KEY           IFEQ      F21
043400980304     C                   MOVEL     '21'          FUNKEY
043500980304     C                   END
043600980304     C     KEY           IFEQ      F22
043700980304     C                   MOVEL     '22'          FUNKEY
043800980304     C                   END
043900980304     C     KEY           IFEQ      F23
044000980304     C                   MOVEL     '23'          FUNKEY
044100980304     C                   END
044200980304     C     KEY           IFEQ      F24
044300980304     C                   MOVEL     '24'          FUNKEY
044400980304     C                   END
044500980304     C     KEY           IFEQ      ROLLUP
044600980304     C                   MOVEL     'RU'          FUNKEY
044700980304     C                   END
044800980304     C     KEY           IFEQ      HELP
044900980304     C                   MOVEL     'HP'          FUNKEY
045000980304     C                   END
045100980304     C                   ENDSR
045200980304     C*------------------------------------------------------------------------*
045300980304     C* NOTE COSTRUZIONE SCHIERE
045400980304     C*------------------------------------------------------------------------*
045500980304     C*
045600980304     C* -MSG-MESSAGGI
045700980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
045800980304     C* (NON TOCCARE MSG,1 E MSG,2)
045900980304     C*
046000980304     C* -CMD-COMANDI
046100980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
046200980304     C*
046300980304     C* -OPD-OPZIONI
046400980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
046500980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
046600980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
046700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
046800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
046900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
047000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
047100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
047200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
047300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
047400980304     C*
047500980304     C* -FUD-FUNZIONI
047600980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
047700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
047800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
047900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
048000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
048300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
048400980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
048500980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
048600980304     o*-------------------------------------------------------------------------
048700980304** msg - messaggi
048800041220TISE71R- Funzione non attiva
048900041220TISE71R- 2
049000041220TISE71R- 3
049100041220TISE71R- 4
049200041220TISE71R- 5
049300041220TISE71R- 6
049400041220TISE71R- 7
049500041220TISE71R- 8
049600041220TISE71R- 9
049700041220TISE71R- 10
049800980304** FUD - FUNZIONI
049900980304                   -        (S/N)=N                        funz.01¬
050000980304                   -        (S/N)=N                        funz.02¬
050100980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
050200980304                   -        (S/N)=N                        funz.04¬
050300980304                   -        (S/N)=N                        funz.05¬
050400980304                   -        (S/N)=N                        funz.06¬
050500980304                   -        (S/N)=N                        funz.07¬
050600980304                   -        (S/N)=N                        funz.08¬
050700980304                   -        (S/N)=N                        funz.09¬
050800980304                   -        (S/N)=N                        funz.10¬
050900980304                   -        (S/N)=N                        funz.11¬
051000980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
051100980304                   -        (S/N)=N                        funz.13¬
051200980304                   -        (S/N)=N                        funz.14¬
051300980304                   -        (S/N)=N                        funz.15¬
051400980304                   -        (S/N)=N                        funz.16¬
051500980304                   -        (S/N)=N                        funz.17¬
051600980304                   -        (S/N)=N                        funz.18¬
051700980304                   -        (S/N)=N                        funz.19¬
051800980304                   -        (S/N)=N                        funz.20¬
051900980304                   -        (S/N)=N                        funz.21¬
052000980304                   -        (S/N)=N                        funz.22¬
052100980304                   -        (S/N)=N                        funz.23¬
052200980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

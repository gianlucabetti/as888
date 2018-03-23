000100980304     H*------------------------------------------------------------------------*
000200020809     H* REPERIMENTO DATI UTENTE
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800020809     FTIBS67D   CF   E             WORKSTN INFDS(DEVDS1)
000900020809     F                                     SFILE(BS67DMS:MSGULT)
001000980304     D*------------------------------------------------------------------------*
001100980304     D* SCHIERE
001200980304     D*------------------------------------------------------------------------*
001300020809     D MSG             S             80    DIM(10) CTDATA PERRCD(1)
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
004800980304     D* ARCHITETTURA
004900980304     D*------------------
005000980304     D KPJBA         E DS
005100020809     D*------------------
005200020809     D* DS X§PARUT
005300020809     D*------------------
005400020809     D UT§DSE0F      E DS
005500020809     D CNCR80        E DS
005600980304     C*------------------------------------------------------------------------*
005700980304     C* MAIN LINES
005800980304     C*------------------------------------------------------------------------*
005900980304     C*
006000980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006100980304DO  1C     F03L00        DOUEQ     '1'
006200980304     C     F12L00        OREQ      '1'
006300980304     C     FINL00        OREQ      '1'
006400980304     C*
006500980304     C* EMETTE FORMATO VIDEO
006600980304     C                   EXSR      BUTFMR
006700980304     C*
006800980304     C* GESTIONE TASTI FUNZIONALI
006900980304     C                   EXSR      FUNCON
007000980304E   1C                   ENDDO
007100980304     C*
007200980304     C                   SETON                                        LR
007300980304     C*------------------------------------------------------------------------*
007400980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007500980304     C*------------------------------------------------------------------------*
007600980304     C     BUTFMR        BEGSR
007700980304     C*
007800980304     C* EMETTE LA TESTATA
007900020809     C                   WRITE     BS67D01
008000980304     C*
008100980304     C* SPEDISCE EVENTUALE MESSAGGIO
008200980304     C                   EXSR      SNDMSG
008300980304     C*
008400980304     C* EMETTE LE FUNZIONI
008500020809     C                   WRITE     BS67D02
008600980304     C*
008700980304     C* EMETTE IL VIDEO
008800020809     C                   EXFMT     BS67D03
008900980304     C*
009000980304     C* REIMPOSTA FLAG DI ERRORE
009100980304     C                   MOVEL     '0'           ERRL00
009200980304     C*
009300980304     C                   ENDSR
009400980304     C*------------------------------------------------------------------------*
009500980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009600980304     C*------------------------------------------------------------------------*
009700980304     C     SNDMSG        BEGSR
009800980304     C*
009900980304     C* PULISCE IL SFILE MESSAGGI
010000980304     C                   MOVEL     '1'           *IN40
010100020809     C                   WRITE     BS67DMC
010200980304     C                   MOVEL     '0'           *IN40
010300980304     C*
010400980304     C* VISUALIZZA IL MESSAGGIO
010500980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
010600980304     C                   CALL      'XMSG'        XMSGPA
010700980304IF  1C     DSMSER        IFEQ      *ZEROS
010800980304     C                   MOVEL     DSMSKE        MSGKEY
010900980304E   1C                   ENDIF
011000980304     C                   Z-ADD     1             MSGULT
011100020809     C                   WRITE     BS67DMS
011200020809     C                   WRITE     BS67DMC
011300980304     C*
011400980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011500980304     C                   MOVEL     *BLANKS       DSMSMS
011600980304     C*
011700980304     C                   ENDSR
011800980304     C*------------------------------------------------------------------------*
011900980304     C* ENTGES - GESTIONE ENTER -> INVIO
012000980304     C*------------------------------------------------------------------------*
012100980304     C     ENTGES        BEGSR
012200980304     C*
012300980304     C*
012400980304     C                   ENDSR
012500980304     C*------------------------------------------------------------------------*
012600980304     C* F03GES - GESTIONE F03 -> FINE
012700980304     C*------------------------------------------------------------------------*
012800980304     C     F03GES        BEGSR
012900980304     C*
013000980304     C* IMPOSTA "PREMUTO F03"
013100980304     C                   MOVEL     '1'           F03L00
013200980304     C*
013300980304     C                   ENDSR
013400020809     C*------------------------------------------------------------------------*
013500020809     C* F06GES - GESTIONE F06 -> ELABORA
013600020809     C*------------------------------------------------------------------------*
013700020809     C     F06GES        BEGSR
013800020809     C*
013900020809     C* LANCIA REPERIMENTO
014000020809     C                   CALL      'TIBS67C'
014100020809     C                   PARM                    VIDTLA
014200020809     C                   PARM                    VIDUTE
014300020809     C                   PARM                    VIDSIF
014400020809     C                   PARM                    KNMUS
014500020809     C                   PARM                    KNSIF
014600020809     C*
014700020809     C* IMPOSTA "PREMUTO F03"
014800020809     C                   MOVEL     '1'           F03L00
014900020809     C*
015000020809     C                   ENDSR
015100980304     C*------------------------------------------------------------------------*
015200980304     C* F12GES - GESTIONE F12 -> RITORNO
015300980304     C*------------------------------------------------------------------------*
015400980304     C     F12GES        BEGSR
015500980304     C*
015600980304     C* IMPOSTA "PREMUTO F12"
015700980304     C                   MOVEL     '1'           F12L00
015800980304     C*
015900980304     C                   ENDSR
016000980304     C*------------------------------------------------------------------------*
016100980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
016200980304     C*------------------------------------------------------------------------*
016300980304     C     F24GES        BEGSR
016400980304     C*
016500980304     C                   EXSR      FUNGES
016600980304     C*
016700980304     C                   ENDSR
016800980304     C*------------------------------------------------------------------------*
016900980304     C* *INZSR - OPERAZIONI INIZIALI
017000980304     C*------------------------------------------------------------------------*
017100980304     C     *INZSR        BEGSR
017200980304     C*---
017300980304     C* RICEVIMENTO PARAMETRI
017400980304     C*---
017500980304     C     *ENTRY        PLIST
017600980304     C                   PARM                    KPJBA
017700980304     C*
017800980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
017900980304     C     XMSGPA        PLIST
018000980304     C                   PARM                    DSMSMS
018100980304     C                   PARM                    DSMSPG
018200980304     C                   PARM                    DSMSKE
018300980304     C                   PARM                    DSMSER
018400980304     C*
018500980304     C* IMPOSTA LA DS -TASTI PREMUTI-
018600980304     C                   CALL      'XKEY'
018700980304     C                   PARM                    DSKEY
018800980304     C*---
018900980304     C* VARIABILI NON RIFERITE AL DATA BASE
019000980304     C*---
019100980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
019200980304     C*---
019300980304     C* VARIABILI DI CONTROLLO
019400980304     C*---
019500980304     C                   MOVEL     '0'           FINL00            1
019600980304     C                   MOVEL     '0'           F03L00            1
019700980304     C                   MOVEL     '0'           F12L00            1
019800980304     C                   MOVEL     '0'           ERRL00            1
019900980304     C*
020000980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
020100020809     C                   MOVEL     'TIBS67R'     DSMSPG
020200020809     C                   MOVEL     'TIBS67R'     MSGQUE
020300980304     C                   MOVEL     *BLANKS       MSGKEY
020400980304     C                   MOVEL     *BLANKS       DSMSMS
020500980304     C*
020600980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
020700980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
020800980304     C                   EXSR      FUNGES
020900020809     C*
021000020809     C* REPERIMENTO DATI UTENTE
021100020809     C                   Z-ADD     1             CODUT
021200020809     C                   CALL      'X§PARUT'
021300020809     C                   PARM                    UT§DSE0F
021400020809     C                   MOVEL     REC80         CNCR80
021500980304     C*
021600980304     C* IMPOSTA IL VIDEO
021700020809     C                   IF        SIMTPP <> 'S'
021800020809     C                   SETON                                        55
021900020809     C                   ENDIF
022000020809     C                   MOVEL     *BLANKS       VIDTLA
022100020809     C                   MOVEL     KNMUS         VIDUTE
022200020809     C                   MOVEL     KNSIF         VIDSIF
022300980304     C*
022400980304     C                   ENDSR
022500980304     C*------------------------------------------------------------------------*
022600980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
022700980304     C*------------------------------------------------------------------------*
022800980304     C     FUNGES        BEGSR
022900980304     C*
023000980304     C* PULISCO CAMPI DI LAVORO
023100980304     C                   Z-ADD     0             W                 5 0
023200980304     C*
023300980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
023400980304     C* E CARICO TABELLA FUNZIONI ATTIVE
023500980304     C     1             DO        24            I
023600980304     C                   MOVEL     FUD(I)        FUDRIG
023700980304     C*
023800980304     C* PULIZIA FUNZIONI PRECEDENTI
023900980304     C                   MOVEL     *BLANKS       FUV(I)
024000980304     C*
024100980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
024200980304     C* (SALTA F24)
024300980304     C     FUDATT        IFEQ      'S'
024400980304     C     FUDFUN        ANDNE     '24'
024500980304     C                   ADD       1             W
024600980304     C                   MOVEL     FUDDES        FUV(W)
024700980304     C                   END
024800980304     C                   END
024900980304     C*
025000980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
025100980304     C* ALTRIMENTI DISATTIVO F24
025200980304     C     1             DO        24            I
025300980304     C                   MOVEL     FUD(I)        FUDRIG
025400980304     C*
025500980304     C     FUDFUN        IFEQ      '24'
025600980304     C     W             IFGT      6
025700980304     C                   MOVEL     'S'           FUDATT
025800980304     C                   ELSE
025900980304     C                   MOVEL     *BLANKS       FUDATT
026000980304     C                   END
026100980304     C                   MOVEL     FUDRIG        FUD(I)
026200980304     C                   END
026300980304     C                   END
026400980304     C*
026500980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
026600980304     C     W             DIV       6             I
026700980304     C                   MVR                     Z
026800980304     C*
026900980304     C     Z             IFNE      0
027000980304     C     I             ADD       1             Z
027100980304     C                   ELSE
027200980304     C                   Z-ADD     I             Z                 5 0
027300980304     C                   END
027400980304     C*
027500980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
027600980304     C                   ADD       1             FUNGRU
027700980304     C     FUNGRU        IFGT      Z
027800980304     C     FUNGRU        ORLT      1
027900980304     C                   Z-ADD     1             FUNGRU
028000980304     C                   END
028100980304     C*
028200980304     C* PRIMO GRUPPO DI 6 FUNZIONI
028300980304     C     FUNGRU        IFEQ      1
028400980304     C                   MOVEL     FUV(1)        FUN001
028500980304     C                   MOVEL     FUV(2)        FUN002
028600980304     C                   MOVEL     FUV(3)        FUN003
028700980304     C                   MOVEL     FUV(4)        FUN004
028800980304     C                   MOVEL     FUV(5)        FUN005
028900980304     C                   MOVEL     FUV(6)        FUN006
029000980304     C                   END
029100980304     C*
029200980304     C* SECONDO GRUPPO DI 6 FUNZIONI
029300980304     C     FUNGRU        IFEQ      2
029400980304     C                   MOVEL     FUV(7)        FUN001
029500980304     C                   MOVEL     FUV(8)        FUN002
029600980304     C                   MOVEL     FUV(9)        FUN003
029700980304     C                   MOVEL     FUV(10)       FUN004
029800980304     C                   MOVEL     FUV(11)       FUN005
029900980304     C                   MOVEL     FUV(12)       FUN006
030000980304     C                   END
030100980304     C*
030200980304     C* TERZO GRUPPO DI 6 FUNZIONI
030300980304     C     FUNGRU        IFEQ      3
030400980304     C                   MOVEL     FUV(13)       FUN001
030500980304     C                   MOVEL     FUV(14)       FUN002
030600980304     C                   MOVEL     FUV(15)       FUN003
030700980304     C                   MOVEL     FUV(16)       FUN004
030800980304     C                   MOVEL     FUV(17)       FUN005
030900980304     C                   MOVEL     FUV(18)       FUN006
031000980304     C                   END
031100980304     C*
031200980304     C* QUARTO GRUPPO DI 6 FUNZIONI
031300980304     C     FUNGRU        IFEQ      4
031400980304     C                   MOVEL     FUV(19)       FUN001
031500980304     C                   MOVEL     FUV(20)       FUN002
031600980304     C                   MOVEL     FUV(21)       FUN003
031700980304     C                   MOVEL     FUV(22)       FUN004
031800980304     C                   MOVEL     *BLANKS       FUN005
031900980304     C                   MOVEL     *BLANKS       FUN006
032000980304     C                   END
032100980304     C*
032200980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
032300980304     C                   MOVEL     *BLANKS       FUN007
032400980304     C                   MOVEL     *BLANKS       FUN008
032500980304     C*
032600980304     C     1             DO        24            I
032700980304     C                   MOVEL     FUD(I)        FUDRIG
032800980304     C     FUDFUN        IFEQ      '24'
032900980304     C     FUDATT        ANDEQ     'S'
033000980304     C                   MOVEL     FUDDES        FUN008
033100980304     C                   END
033200980304     C                   END
033300980304     C*
033400980304     C                   ENDSR
033500980304     C*------------------------------------------------------------------------*
033600980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
033700980304     C*------------------------------------------------------------------------*
033800980304     C     FUNCON        BEGSR
033900980304     C*
034000980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
034100980304     C                   EXSR      FUNVAR
034200980304     C*
034300980304     C* PULISCO CAMPI DI LAVORO
034400980304     C                   Z-ADD     0             I                 5 0
034500980304     C                   Z-ADD     0             W                 5 0
034600980304     C*
034700980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
034800980304     C     FUNKEY        IFNE      *BLANKS
034900980304     C     1             DO        24            I
035000980304     C                   MOVEL     FUD(I)        FUDRIG
035100980304     C*
035200980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
035300980304     C     FUNKEY        IFEQ      FUDFUN
035400980304     C     FUDATT        ANDEQ     'S'
035500980304     C                   Z-ADD     I             W
035600980304     C                   END
035700980304     C                   END
035800980304     C*
035900980304     C* FUNZIONE NON ATTIVA (ERRORE)
036000980304     C     W             IFEQ      0
036100980304     C*
036200980304     C* EMISSIONE MESSAGGIO DI ERRORE
036300980304     C                   MOVEL     MSG(1)        DSMSMS
036400980304     C*
036500980304     C                   END
036600980304     C                   END
036700980304     C*
036800980304     C* FUNZIONE ATTIVA (PROCEDI ...)
036900980304     C     W             IFNE      0
037000980304     C     FUNKEY        OREQ      *BLANKS
037100980304     C     FUNKEY        IFEQ      *BLANKS
037200980304     C                   EXSR      ENTGES
037300980304     C                   END
037400980304     C     FUNKEY        IFEQ      '03'
037500980304     C                   EXSR      F03GES
037600980304     C                   END
037700020809     C     FUNKEY        IFEQ      '06'
037800020809     C                   EXSR      F06GES
037900020809     C                   END
038000980304     C     FUNKEY        IFEQ      '12'
038100980304     C                   EXSR      F12GES
038200980304     C                   END
038300980304     C                   END
038400980304     C*
038500980304     C                   ENDSR
038600980304     C*------------------------------------------------------------------------*
038700980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
038800980304     C*------------------------------------------------------------------------*
038900980304     C     FUNVAR        BEGSR
039000980304     C*
039100980304     C* INIZIALIZZA VARIABILE -FUNKEY-
039200980304     C*
039300980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
039400980304     C     KEY           IFEQ      ENTER
039500980304     C                   MOVEL     *BLANKS       FUNKEY
039600980304     C                   END
039700980304     C     KEY           IFEQ      F1
039800980304     C                   MOVEL     '01'          FUNKEY
039900980304     C                   END
040000980304     C     KEY           IFEQ      F2
040100980304     C                   MOVEL     '02'          FUNKEY
040200980304     C                   END
040300980304     C     KEY           IFEQ      F3
040400980304     C                   MOVEL     '03'          FUNKEY
040500980304     C                   END
040600980304     C     KEY           IFEQ      F4
040700980304     C                   MOVEL     '04'          FUNKEY
040800980304     C                   END
040900980304     C     KEY           IFEQ      F5
041000980304     C                   MOVEL     '05'          FUNKEY
041100980304     C                   END
041200980304     C     KEY           IFEQ      F6
041300980304     C                   MOVEL     '06'          FUNKEY
041400980304     C                   END
041500980304     C     KEY           IFEQ      F7
041600980304     C                   MOVEL     '07'          FUNKEY
041700980304     C                   END
041800980304     C     KEY           IFEQ      F8
041900980304     C                   MOVEL     '08'          FUNKEY
042000980304     C                   END
042100980304     C     KEY           IFEQ      F9
042200980304     C                   MOVEL     '09'          FUNKEY
042300980304     C                   END
042400980304     C     KEY           IFEQ      F10
042500980304     C                   MOVEL     '10'          FUNKEY
042600980304     C                   END
042700980304     C     KEY           IFEQ      F11
042800980304     C                   MOVEL     '11'          FUNKEY
042900980304     C                   END
043000980304     C     KEY           IFEQ      F12
043100980304     C                   MOVEL     '12'          FUNKEY
043200980304     C                   END
043300980304     C     KEY           IFEQ      F13
043400980304     C                   MOVEL     '13'          FUNKEY
043500980304     C                   END
043600980304     C     KEY           IFEQ      F14
043700980304     C                   MOVEL     '14'          FUNKEY
043800980304     C                   END
043900980304     C     KEY           IFEQ      F15
044000980304     C                   MOVEL     '15'          FUNKEY
044100980304     C                   END
044200980304     C     KEY           IFEQ      F16
044300980304     C                   MOVEL     '16'          FUNKEY
044400980304     C                   END
044500980304     C     KEY           IFEQ      F17
044600980304     C                   MOVEL     '17'          FUNKEY
044700980304     C                   END
044800980304     C     KEY           IFEQ      F18
044900980304     C                   MOVEL     '18'          FUNKEY
045000980304     C                   END
045100980304     C     KEY           IFEQ      F19
045200980304     C                   MOVEL     '19'          FUNKEY
045300980304     C                   END
045400980304     C     KEY           IFEQ      F20
045500980304     C                   MOVEL     '20'          FUNKEY
045600980304     C                   END
045700980304     C     KEY           IFEQ      F21
045800980304     C                   MOVEL     '21'          FUNKEY
045900980304     C                   END
046000980304     C     KEY           IFEQ      F22
046100980304     C                   MOVEL     '22'          FUNKEY
046200980304     C                   END
046300980304     C     KEY           IFEQ      F23
046400980304     C                   MOVEL     '23'          FUNKEY
046500980304     C                   END
046600980304     C     KEY           IFEQ      F24
046700980304     C                   MOVEL     '24'          FUNKEY
046800980304     C                   END
046900980304     C     KEY           IFEQ      ROLLUP
047000980304     C                   MOVEL     'RU'          FUNKEY
047100980304     C                   END
047200980304     C     KEY           IFEQ      HELP
047300980304     C                   MOVEL     'HP'          FUNKEY
047400980304     C                   END
047500980304     C                   ENDSR
047600980304     C*------------------------------------------------------------------------*
047700980304     C* NOTE COSTRUZIONE SCHIERE
047800980304     C*------------------------------------------------------------------------*
047900980304     C*
048000980304     C* -MSG-MESSAGGI
048100980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
048200980304     C* (NON TOCCARE MSG,1 E MSG,2)
048300980304     C*
048400980304     C* -CMD-COMANDI
048500980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
048600980304     C*
048700980304     C* -OPD-OPZIONI
048800980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
048900980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
049000980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
049100980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
049200980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
049300980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
049400980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049500980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049600980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049700980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
049800980304     C*
049900980304     C* -FUD-FUNZIONI
050000980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
050100980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
050200980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
050300980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
050400980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
050500980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
050600980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
050700980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
050800980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
050900980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
051000980304     o*-------------------------------------------------------------------------
051100980304** msg - messaggi
051200020809TIBS67R- Funzione non attiva
051300020809TIBS67R- 2
051400020809TIBS67R- 3
051500020809TIBS67R- 4
051600020809TIBS67R- 5
051700020809TIBS67R- 6
051800020809TIBS67R- 7
051900020809TIBS67R- 8
052000020809TIBS67R- 9
052100020809TIBS67R- 10
052200980304** FUD - FUNZIONI
052300980304                   -        (S/N)=N                        funz.01¬
052400980304                   -        (S/N)=N                        funz.02¬
052500980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
052600980304                   -        (S/N)=N                        funz.04¬
052700980304                   -        (S/N)=N                        funz.05¬
052800020809F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
052900980304                   -        (S/N)=N                        funz.07¬
053000980304                   -        (S/N)=N                        funz.08¬
053100980304                   -        (S/N)=N                        funz.09¬
053200980304                   -        (S/N)=N                        funz.10¬
053300980304                   -        (S/N)=N                        funz.11¬
053400020809F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
053500980304                   -        (S/N)=N                        funz.13¬
053600980304                   -        (S/N)=N                        funz.14¬
053700980304                   -        (S/N)=N                        funz.15¬
053800980304                   -        (S/N)=N                        funz.16¬
053900980304                   -        (S/N)=N                        funz.17¬
054000980304                   -        (S/N)=N                        funz.18¬
054100980304                   -        (S/N)=N                        funz.19¬
054200980304                   -        (S/N)=N                        funz.20¬
054300980304                   -        (S/N)=N                        funz.21¬
054400980304                   -        (S/N)=N                        funz.22¬
054500980304                   -        (S/N)=N                        funz.23¬
054600980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

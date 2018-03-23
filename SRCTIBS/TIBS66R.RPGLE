000100980304     H*------------------------------------------------------------------------*
000200980304     H* REPERIMENTO TERMINALs - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800020731     FTIBS66D   CF   E             WORKSTN INFDS(DEVDS1)
000900020731     F                                     SFILE(BS66DMS:MSGULT)
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
004700980304     D*------------
004800980304     D* DS "XFIL" - REPERIMENTO DATI UTENTE
004900980304     D*------------
005000980304     D DSFIL         E DS
005100980304     D*------------------
005200980304     D* DS di procedura
005300980304     D*------------------
005400020731     D TRUL06DS      E DS
005500021028     D  LIA                    1     90    DIM(30)                               -filiali
005600980304     D*------------------
005700980304     D* ARCHITETTURA
005800980304     D*------------------
005900980304     D KPJBA         E DS
006000980304     C*------------------------------------------------------------------------*
006100980304     C* MAIN LINES
006200980304     C*------------------------------------------------------------------------*
006300980304     C*
006400980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006500980304DO  1C     F03L00        DOUEQ     '1'
006600980304     C     F12L00        OREQ      '1'
006700980304     C     FINL00        OREQ      '1'
006800980304     C*
006900980304     C* EMETTE FORMATO VIDEO
007000980304     C                   EXSR      BUTFMR
007100980304     C*
007200980304     C* GESTIONE TASTI FUNZIONALI
007300980304     C                   EXSR      FUNCON
007400980304E   1C                   ENDDO
007500980304     C*
007600980304     C                   SETON                                        LR
007700980304     C*------------------------------------------------------------------------*
007800980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007900980304     C*------------------------------------------------------------------------*
008000980304     C     BUTFMR        BEGSR
008100980304     C*
008200980304     C* EMETTE LA TESTATA
008300020731     C                   WRITE     BS66D01
008400980304     C*
008500980304     C* SPEDISCE EVENTUALE MESSAGGIO
008600980304     C                   EXSR      SNDMSG
008700980304     C*
008800980304     C* EMETTE LE FUNZIONI
008900020731     C                   WRITE     BS66D02
009000980304     C*
009100980304     C* EMETTE IL VIDEO
009200020731     C                   EXFMT     BS66D03
009300980304     C*
009400980304     C* REIMPOSTA FLAG DI ERRORE
009500980304     C                   MOVEL     '0'           ERRL00
009600980304     C*
009700980304     C* REIMPOSTA INDICATORI DI ERRORE
009800980304     C                   SETOFF                                       59
009900980304     C                   SETOFF                                       606162
010000980304     C                   SETOFF                                       636465
010100980304     C                   SETOFF                                       666768
010200980304     C                   SETOFF                                       697071
010300980304     C                   SETOFF                                       727374
010400980304     C                   SETOFF                                       757677
010500980304     C                   SETOFF                                       787980
010600980304     C                   SETOFF                                       818283
010700980304     C                   SETOFF                                       848586
010800980304     C                   SETOFF                                       878889
010900980304     C*
011000980304     C                   ENDSR
011100980304     C*------------------------------------------------------------------------*
011200980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011300980304     C*------------------------------------------------------------------------*
011400980304     C     SNDMSG        BEGSR
011500980304     C*
011600980304     C* PULISCE IL SFILE MESSAGGI
011700980304     C                   MOVEL     '1'           *IN40
011800020731     C                   WRITE     BS66DMC
011900980304     C                   MOVEL     '0'           *IN40
012000980304     C*
012100980304     C* VISUALIZZA IL MESSAGGIO
012200980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
012300980304     C                   CALL      'XMSG'        XMSGPA
012400980304IF  1C     DSMSER        IFEQ      *ZEROS
012500980304     C                   MOVEL     DSMSKE        MSGKEY
012600980304E   1C                   ENDIF
012700980304     C                   Z-ADD     1             MSGULT
012800020731     C                   WRITE     BS66DMS
012900020731     C                   WRITE     BS66DMC
013000980304     C*
013100980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013200980304     C                   MOVEL     *BLANKS       DSMSMS
013300980304     C*
013400980304     C                   ENDSR
013500980304     C*------------------------------------------------------------------------*
013600980304     C* ENTGES - GESTIONE ENTER -> INVIO
013700980304     C*------------------------------------------------------------------------*
013800980304     C     ENTGES        BEGSR
013900980304     C*
014000020731     c                   MOVEL     TRUL06DS      KPJBU
014100020731     c                   CALL      'TRUL06R'
014200020731     c                   PARM                    KPJBA
014300020731     c                   MOVEL     KPJBU         TRUL06DS
014400021028     C*
014500021028     C* Manipolo la schiera del risultato x 1 + efficace visualizzazione a video
014600021028     C                   MOVEL     *blanks       W06LIA
014700021028     C                   Z-ADD     1             JJ                3 0
014800021028     C*
014900021028     C* La lunghezza totale del campo a video è:
015000021028     C*  3  (lunghezza di ogni elento d schiera contenente la filiale) *
015100021028     C*  30 (il numeo degli elemneti della schiera)                    +
015200021028     C*  29 (il totale degli spazi lasciando 1 spazio tra ogni elem.)
015300021028     C*
015400021028     C                   DOW       JJ <= 30
015500021028     C                   IF        LIA(JJ) > *all'0'
015600021028     C                   EVAL      W06LIA = %trim(W06LIA) + ' ' + LIA(JJ)
015700021028     C                   ENDIF
015800021028     C                   ADD       1             JJ
015900021028     C                   ENDDO
016000980304     C*
016100980304     C                   ENDSR
016200980304     C*------------------------------------------------------------------------*
016300980304     C* F03GES - GESTIONE F03 -> FINE
016400980304     C*------------------------------------------------------------------------*
016500980304     C     F03GES        BEGSR
016600980304     C*
016700980304     C* IMPOSTA "PREMUTO F03"
016800980304     C                   MOVEL     '1'           F03L00
016900980304     C*
017000980304     C                   ENDSR
017100980304     C*------------------------------------------------------------------------*
017200980304     C* F12GES - GESTIONE F12 -> RITORNO
017300980304     C*------------------------------------------------------------------------*
017400980304     C     F12GES        BEGSR
017500980304     C*
017600980304     C* IMPOSTA "PREMUTO F12"
017700980304     C                   MOVEL     '1'           F12L00
017800980304     C*
017900980304     C                   ENDSR
018000980304     C*------------------------------------------------------------------------*
018100980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
018200980304     C*------------------------------------------------------------------------*
018300980304     C     F24GES        BEGSR
018400980304     C*
018500980304     C                   EXSR      FUNGES
018600980304     C*
018700980304     C                   ENDSR
018800980304     C*------------------------------------------------------------------------*
018900980304     C* *INZSR - OPERAZIONI INIZIALI
019000980304     C*------------------------------------------------------------------------*
019100980304     C     *INZSR        BEGSR
019200980304     C*---
019300980304     C* RICEVIMENTO PARAMETRI
019400980304     C*---
019500980304     C     *ENTRY        PLIST
019600980304     C                   PARM                    KPJBA
019700980304     C*
019800980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
019900980304     C     XMSGPA        PLIST
020000980304     C                   PARM                    DSMSMS
020100980304     C                   PARM                    DSMSPG
020200980304     C                   PARM                    DSMSKE
020300980304     C                   PARM                    DSMSER
020400980304     C*
020500980304     C* IMPOSTA LA DS -TASTI PREMUTI-
020600980304     C                   CALL      'XKEY'
020700980304     C                   PARM                    DSKEY
020800980304     C*---
020900980304     C* VARIABILI NON RIFERITE AL DATA BASE
021000980304     C*---
021100980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
021200980304     C*---
021300980304     C* VARIABILI DI CONTROLLO
021400980304     C*---
021500980304     C                   MOVEL     '0'           FINL00            1
021600980304     C                   MOVEL     '0'           F03L00            1
021700980304     C                   MOVEL     '0'           F12L00            1
021800980304     C                   MOVEL     '0'           ERRL00            1
021900980304     C*
022000980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
022100020731     C                   MOVEL     'TIBS66R'     DSMSPG
022200020731     C                   MOVEL     'TIBS66R'     MSGQUE
022300980304     C                   MOVEL     *BLANKS       MSGKEY
022400980304     C                   MOVEL     *BLANKS       DSMSMS
022500980304     C*
022600980304     C* REPERIMENTO DATI UTENTE
022700980304     C                   CALL      'XFIL'
022800980304     C                   PARM                    DSFIL
022900980304     C*
023000980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
023100980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
023200980304     C                   EXSR      FUNGES
023300980304     C*
023400980304     C* IMPOSTA IL VIDEO
023500020731     c                   CLEAR                   TRUL06DS
023600980304     C*
023700980304     C                   ENDSR
023800980304     C*------------------------------------------------------------------------*
023900980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
024000980304     C*------------------------------------------------------------------------*
024100980304     C     FUNGES        BEGSR
024200980304     C*
024300980304     C* PULISCO CAMPI DI LAVORO
024400980304     C                   Z-ADD     0             W                 5 0
024500980304     C*
024600980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
024700980304     C* E CARICO TABELLA FUNZIONI ATTIVE
024800980304     C     1             DO        24            I
024900980304     C                   MOVEL     FUD(I)        FUDRIG
025000980304     C*
025100980304     C* PULIZIA FUNZIONI PRECEDENTI
025200980304     C                   MOVEL     *BLANKS       FUV(I)
025300980304     C*
025400980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
025500980304     C* (SALTA F24)
025600980304     C     FUDATT        IFEQ      'S'
025700980304     C     FUDFUN        ANDNE     '24'
025800980304     C                   ADD       1             W
025900980304     C                   MOVEL     FUDDES        FUV(W)
026000980304     C                   END
026100980304     C                   END
026200980304     C*
026300980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
026400980304     C* ALTRIMENTI DISATTIVO F24
026500980304     C     1             DO        24            I
026600980304     C                   MOVEL     FUD(I)        FUDRIG
026700980304     C*
026800980304     C     FUDFUN        IFEQ      '24'
026900980304     C     W             IFGT      6
027000980304     C                   MOVEL     'S'           FUDATT
027100980304     C                   ELSE
027200980304     C                   MOVEL     *BLANKS       FUDATT
027300980304     C                   END
027400980304     C                   MOVEL     FUDRIG        FUD(I)
027500980304     C                   END
027600980304     C                   END
027700980304     C*
027800980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
027900980304     C     W             DIV       6             I
028000980304     C                   MVR                     Z
028100980304     C*
028200980304     C     Z             IFNE      0
028300980304     C     I             ADD       1             Z
028400980304     C                   ELSE
028500980304     C                   Z-ADD     I             Z                 5 0
028600980304     C                   END
028700980304     C*
028800980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
028900980304     C                   ADD       1             FUNGRU
029000980304     C     FUNGRU        IFGT      Z
029100980304     C     FUNGRU        ORLT      1
029200980304     C                   Z-ADD     1             FUNGRU
029300980304     C                   END
029400980304     C*
029500980304     C* PRIMO GRUPPO DI 6 FUNZIONI
029600980304     C     FUNGRU        IFEQ      1
029700980304     C                   MOVEL     FUV(1)        FUN001
029800980304     C                   MOVEL     FUV(2)        FUN002
029900980304     C                   MOVEL     FUV(3)        FUN003
030000980304     C                   MOVEL     FUV(4)        FUN004
030100980304     C                   MOVEL     FUV(5)        FUN005
030200980304     C                   MOVEL     FUV(6)        FUN006
030300980304     C                   END
030400980304     C*
030500980304     C* SECONDO GRUPPO DI 6 FUNZIONI
030600980304     C     FUNGRU        IFEQ      2
030700980304     C                   MOVEL     FUV(7)        FUN001
030800980304     C                   MOVEL     FUV(8)        FUN002
030900980304     C                   MOVEL     FUV(9)        FUN003
031000980304     C                   MOVEL     FUV(10)       FUN004
031100980304     C                   MOVEL     FUV(11)       FUN005
031200980304     C                   MOVEL     FUV(12)       FUN006
031300980304     C                   END
031400980304     C*
031500980304     C* TERZO GRUPPO DI 6 FUNZIONI
031600980304     C     FUNGRU        IFEQ      3
031700980304     C                   MOVEL     FUV(13)       FUN001
031800980304     C                   MOVEL     FUV(14)       FUN002
031900980304     C                   MOVEL     FUV(15)       FUN003
032000980304     C                   MOVEL     FUV(16)       FUN004
032100980304     C                   MOVEL     FUV(17)       FUN005
032200980304     C                   MOVEL     FUV(18)       FUN006
032300980304     C                   END
032400980304     C*
032500980304     C* QUARTO GRUPPO DI 6 FUNZIONI
032600980304     C     FUNGRU        IFEQ      4
032700980304     C                   MOVEL     FUV(19)       FUN001
032800980304     C                   MOVEL     FUV(20)       FUN002
032900980304     C                   MOVEL     FUV(21)       FUN003
033000980304     C                   MOVEL     FUV(22)       FUN004
033100980304     C                   MOVEL     *BLANKS       FUN005
033200980304     C                   MOVEL     *BLANKS       FUN006
033300980304     C                   END
033400980304     C*
033500980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
033600980304     C                   MOVEL     *BLANKS       FUN007
033700980304     C                   MOVEL     *BLANKS       FUN008
033800980304     C*
033900980304     C     1             DO        24            I
034000980304     C                   MOVEL     FUD(I)        FUDRIG
034100980304     C     FUDFUN        IFEQ      '24'
034200980304     C     FUDATT        ANDEQ     'S'
034300980304     C                   MOVEL     FUDDES        FUN008
034400980304     C                   END
034500980304     C                   END
034600980304     C*
034700980304     C                   ENDSR
034800980304     C*------------------------------------------------------------------------*
034900980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
035000980304     C*------------------------------------------------------------------------*
035100980304     C     FUNCON        BEGSR
035200980304     C*
035300980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
035400980304     C                   EXSR      FUNVAR
035500980304     C*
035600980304     C* PULISCO CAMPI DI LAVORO
035700980304     C                   Z-ADD     0             I                 5 0
035800980304     C                   Z-ADD     0             W                 5 0
035900980304     C*
036000980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
036100980304     C     FUNKEY        IFNE      *BLANKS
036200980304     C     1             DO        24            I
036300980304     C                   MOVEL     FUD(I)        FUDRIG
036400980304     C*
036500980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
036600980304     C     FUNKEY        IFEQ      FUDFUN
036700980304     C     FUDATT        ANDEQ     'S'
036800980304     C                   Z-ADD     I             W
036900980304     C                   END
037000980304     C                   END
037100980304     C*
037200980304     C* FUNZIONE NON ATTIVA (ERRORE)
037300980304     C     W             IFEQ      0
037400980304     C*
037500980304     C* EMISSIONE MESSAGGIO DI ERRORE
037600980304     C                   MOVEL     MSG(1)        DSMSMS
037700980304     C*
037800980304     C                   END
037900980304     C                   END
038000980304     C*
038100980304     C* FUNZIONE ATTIVA (PROCEDI ...)
038200980304     C     W             IFNE      0
038300980304     C     FUNKEY        OREQ      *BLANKS
038400980304     C     FUNKEY        IFEQ      *BLANKS
038500980304     C                   EXSR      ENTGES
038600980304     C                   END
038700980304     C     FUNKEY        IFEQ      '03'
038800980304     C                   EXSR      F03GES
038900980304     C                   END
039000980304     C     FUNKEY        IFEQ      '12'
039100980304     C                   EXSR      F12GES
039200980304     C                   END
039300980304     C                   END
039400980304     C*
039500980304     C                   ENDSR
039600980304     C*------------------------------------------------------------------------*
039700980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
039800980304     C*------------------------------------------------------------------------*
039900980304     C     AZZVID        BEGSR
040000980304     C*
040100980304     C                   ENDSR
040200980304     C*------------------------------------------------------------------------*
040300980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
040400980304     C*------------------------------------------------------------------------*
040500980304     C     FUNVAR        BEGSR
040600980304     C*
040700980304     C* INIZIALIZZA VARIABILE -FUNKEY-
040800980304     C*
040900980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
041000980304     C     KEY           IFEQ      ENTER
041100980304     C                   MOVEL     *BLANKS       FUNKEY
041200980304     C                   END
041300980304     C     KEY           IFEQ      F1
041400980304     C                   MOVEL     '01'          FUNKEY
041500980304     C                   END
041600980304     C     KEY           IFEQ      F2
041700980304     C                   MOVEL     '02'          FUNKEY
041800980304     C                   END
041900980304     C     KEY           IFEQ      F3
042000980304     C                   MOVEL     '03'          FUNKEY
042100980304     C                   END
042200980304     C     KEY           IFEQ      F4
042300980304     C                   MOVEL     '04'          FUNKEY
042400980304     C                   END
042500980304     C     KEY           IFEQ      F5
042600980304     C                   MOVEL     '05'          FUNKEY
042700980304     C                   END
042800980304     C     KEY           IFEQ      F6
042900980304     C                   MOVEL     '06'          FUNKEY
043000980304     C                   END
043100980304     C     KEY           IFEQ      F7
043200980304     C                   MOVEL     '07'          FUNKEY
043300980304     C                   END
043400980304     C     KEY           IFEQ      F8
043500980304     C                   MOVEL     '08'          FUNKEY
043600980304     C                   END
043700980304     C     KEY           IFEQ      F9
043800980304     C                   MOVEL     '09'          FUNKEY
043900980304     C                   END
044000980304     C     KEY           IFEQ      F10
044100980304     C                   MOVEL     '10'          FUNKEY
044200980304     C                   END
044300980304     C     KEY           IFEQ      F11
044400980304     C                   MOVEL     '11'          FUNKEY
044500980304     C                   END
044600980304     C     KEY           IFEQ      F12
044700980304     C                   MOVEL     '12'          FUNKEY
044800980304     C                   END
044900980304     C     KEY           IFEQ      F13
045000980304     C                   MOVEL     '13'          FUNKEY
045100980304     C                   END
045200980304     C     KEY           IFEQ      F14
045300980304     C                   MOVEL     '14'          FUNKEY
045400980304     C                   END
045500980304     C     KEY           IFEQ      F15
045600980304     C                   MOVEL     '15'          FUNKEY
045700980304     C                   END
045800980304     C     KEY           IFEQ      F16
045900980304     C                   MOVEL     '16'          FUNKEY
046000980304     C                   END
046100980304     C     KEY           IFEQ      F17
046200980304     C                   MOVEL     '17'          FUNKEY
046300980304     C                   END
046400980304     C     KEY           IFEQ      F18
046500980304     C                   MOVEL     '18'          FUNKEY
046600980304     C                   END
046700980304     C     KEY           IFEQ      F19
046800980304     C                   MOVEL     '19'          FUNKEY
046900980304     C                   END
047000980304     C     KEY           IFEQ      F20
047100980304     C                   MOVEL     '20'          FUNKEY
047200980304     C                   END
047300980304     C     KEY           IFEQ      F21
047400980304     C                   MOVEL     '21'          FUNKEY
047500980304     C                   END
047600980304     C     KEY           IFEQ      F22
047700980304     C                   MOVEL     '22'          FUNKEY
047800980304     C                   END
047900980304     C     KEY           IFEQ      F23
048000980304     C                   MOVEL     '23'          FUNKEY
048100980304     C                   END
048200980304     C     KEY           IFEQ      F24
048300980304     C                   MOVEL     '24'          FUNKEY
048400980304     C                   END
048500980304     C     KEY           IFEQ      ROLLUP
048600980304     C                   MOVEL     'RU'          FUNKEY
048700980304     C                   END
048800980304     C     KEY           IFEQ      HELP
048900980304     C                   MOVEL     'HP'          FUNKEY
049000980304     C                   END
049100980304     C                   ENDSR
049200980304     C*------------------------------------------------------------------------*
049300980304     C* NOTE COSTRUZIONE SCHIERE
049400980304     C*------------------------------------------------------------------------*
049500980304     C*
049600980304     C* -MSG-MESSAGGI
049700980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
049800980304     C* (NON TOCCARE MSG,1 E MSG,2)
049900980304     C*
050000980304     C* -CMD-COMANDI
050100980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
050200980304     C*
050300980304     C* -OPD-OPZIONI
050400980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
050500980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
050600980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
050700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
050800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
050900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
051000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
051100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
051200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
051300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
051400980304     C*
051500980304     C* -FUD-FUNZIONI
051600980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
051700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
051800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
051900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
052000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
052100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
052200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
052300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
052400980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
052500980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
052600980304     o*-------------------------------------------------------------------------
052700980304** msg - messaggi
052800020731TIBS66R- Funzione non attiva
052900020731TIBS66R- 2
053000020731TIBS66R- 3
053100020731TIBS66R- 4
053200020731TIBS66R- 5
053300020731TIBS66R- 6
053400020731TIBS66R- 7
053500020731TIBS66R- 8
053600020731TIBS66R- 9
053700020731TIBS66R- 10
053800980304** FUD - FUNZIONI
053900980304                   -        (S/N)=N                        funz.01¬
054000980304                   -        (S/N)=N                        funz.02¬
054100980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
054200980304                   -        (S/N)=N                        funz.04¬
054300980304                   -        (S/N)=N                        funz.05¬
054400980304                   -        (S/N)=N                        funz.06¬
054500980304                   -        (S/N)=N                        funz.07¬
054600980304                   -        (S/N)=N                        funz.08¬
054700980304                   -        (S/N)=N                        funz.09¬
054800980304                   -        (S/N)=N                        funz.10¬
054900980304                   -        (S/N)=N                        funz.11¬
055000980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
055100980304                   -        (S/N)=N                        funz.13¬
055200980304                   -        (S/N)=N                        funz.14¬
055300980304                   -        (S/N)=N                        funz.15¬
055400980304                   -        (S/N)=N                        funz.16¬
055500980304                   -        (S/N)=N                        funz.17¬
055600980304                   -        (S/N)=N                        funz.18¬
055700980304                   -        (S/N)=N                        funz.19¬
055800980304                   -        (S/N)=N                        funz.20¬
055900980304                   -        (S/N)=N                        funz.21¬
056000980304                   -        (S/N)=N                        funz.22¬
056100980304                   -        (S/N)=N                        funz.23¬
056200980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

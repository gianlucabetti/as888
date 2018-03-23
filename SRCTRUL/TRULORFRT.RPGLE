000100980304     H*------------------------------------------------------------------------*
000200140929     H* verifica reperimento dati filiale - TESTER
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800140929     Ftrulorfdt CF   E             WORKSTN INFDS(DEVDS1)
000900140929     F                                     SFILE(ULORfTDMS:MSGULT)
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
005000140929     d trulorfds     E DS
005100140929     d trulorfdso    E DS
005200140310     d
005300980304     D*------------------
005400980304     D* ARCHITETTURA
005500980304     D*------------------
005600980304     D KPJBA         E DS
005700980304     C*------------------------------------------------------------------------*
005800980304     C* MAIN LINES
005900980304     C*------------------------------------------------------------------------*
006000980304     C*
006100980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006200980304DO  1C     F03L00        DOUEQ     '1'
006300980304     C     F12L00        OREQ      '1'
006400980304     C     FINL00        OREQ      '1'
006500980304     C*
006600980304     C* EMETTE FORMATO VIDEO
006700980304     C                   EXSR      BUTFMR
006800980304     C*
006900980304     C* GESTIONE TASTI FUNZIONALI
007000980304     C                   EXSR      FUNCON
007100980304E   1C                   ENDDO
007200140310     c
007300980304     C*
007400980304     C                   SETON                                        LR
007500980304     C*------------------------------------------------------------------------*
007600980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007700980304     C*------------------------------------------------------------------------*
007800980304     C     BUTFMR        BEGSR
007900980304     C*
008000980304     C* EMETTE LA TESTATA
008100140929     C                   WRITE     ULORFTD01
008200980304     C*
008300980304     C* SPEDISCE EVENTUALE MESSAGGIO
008400980304     C                   EXSR      SNDMSG
008500980304     C*
008600980304     C* EMETTE LE FUNZIONI
008700140929     C                   WRITE     ULORfTD02
008800980304     C*
008900980304     C* EMETTE IL VIDEO
009000140929     C                   EXFMT     ULORfTD03
009100980304     C*
009200980304     C* REIMPOSTA FLAG DI ERRORE
009300980304     C                   MOVEL     '0'           ERRL00
009400980304     C*
009500980304     C* REIMPOSTA INDICATORI DI ERRORE
009600980304     C*
009700980304     C                   ENDSR
009800980304     C*------------------------------------------------------------------------*
009900980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010000980304     C*------------------------------------------------------------------------*
010100980304     C     SNDMSG        BEGSR
010200980304     C*
010300980304     C* PULISCE IL SFILE MESSAGGI
010400980304     C                   MOVEL     '1'           *IN40
010500140929     C                   WRITE     ULORfTDMC
010600980304     C                   MOVEL     '0'           *IN40
010700980304     C*
010800980304     C* VISUALIZZA IL MESSAGGIO
010900980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
011000980304     C                   CALL      'XMSG'        XMSGPA
011100980304IF  1C     DSMSER        IFEQ      *ZEROS
011200980304     C                   MOVEL     DSMSKE        MSGKEY
011300980304E   1C                   ENDIF
011400980304     C                   Z-ADD     1             MSGULT
011500140929     C                   WRITE     ULORfTDMS
011600140929     C                   WRITE     ULORfTDMC
011700980304     C*
011800980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011900980304     C                   MOVEL     *BLANKS       DSMSMS
012000980304     C*
012100980304     C                   ENDSR
012200980304     C*------------------------------------------------------------------------*
012300980304     C* ENTGES - GESTIONE ENTER -> INVIO
012400980304     C*------------------------------------------------------------------------*
012500980304     C     ENTGES        BEGSR
012600140929     c                   clear                   trulorfds
012700140929     c                   clear                   trulorfdso
012800140929     c                   eval      iorflin=vidlin
012900140929     c                   eval      iorffil=vidfil
013000980304     C*
013100140929    3c                   if        vidtipo='V' or
013200140929    3c                             vidtipo=' '
013300140929     c**                 eval      IORfcol = 35
013400140929     c**                 eval      IORfrig = 8
013500140317     c
013600140929     c                   write     ULORfTD03
013700140317     c
013800140929     c                   CALL      'TRULORFR'
013900140310     c                   PARM                    kpjba
014000140929     c                   PARM                    trulorfds
014100140311     c
014200140317   x3c                   else
014300140311     c*
014400140929     c                   CALL      'TRULORFR'
014500140310     c                   PARM                    kpjba
014600140929     c                   PARM                    trulorFds
014700140929     c                   PARM                    trulorfdso
014800140317    3c                   endif
014900980304     C*
015000980304     C                   ENDSR
015100980304     C*------------------------------------------------------------------------*
015200980304     C* F03GES - GESTIONE F03 -> FINE
015300980304     C*------------------------------------------------------------------------*
015400980304     C     F03GES        BEGSR
015500980304     C*
015600980304     C* IMPOSTA "PREMUTO F03"
015700980304     C                   MOVEL     '1'           F03L00
015800980304     C*
015900980304     C                   ENDSR
016000980304     C*------------------------------------------------------------------------*
016100980304     C* F12GES - GESTIONE F12 -> RITORNO
016200980304     C*------------------------------------------------------------------------*
016300980304     C     F12GES        BEGSR
016400980304     C*
016500980304     C* IMPOSTA "PREMUTO F12"
016600980304     C                   MOVEL     '1'           F12L00
016700980304     C*
016800980304     C                   ENDSR
016900140317     C*------------------------------------------------------------------------*
017000140317     C* F10GES - pulisce
017100140317     C*------------------------------------------------------------------------*
017200140317     C     F10GES        BEGSR
017300140317     C*
017400140317     C                   ENDSR
017500980304     C*------------------------------------------------------------------------*
017600980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017700980304     C*------------------------------------------------------------------------*
017800980304     C     F24GES        BEGSR
017900980304     C*
018000980304     C                   EXSR      FUNGES
018100980304     C*
018200980304     C                   ENDSR
018300980304     C*------------------------------------------------------------------------*
018400980304     C* *INZSR - OPERAZIONI INIZIALI
018500980304     C*------------------------------------------------------------------------*
018600980304     C     *INZSR        BEGSR
018700980304     C*---
018800980304     C* RICEVIMENTO PARAMETRI
018900980304     C*---
019000980304     C     *ENTRY        PLIST
019100980304     C                   PARM                    KPJBA
019200980304     C*
019300980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
019400980304     C     XMSGPA        PLIST
019500980304     C                   PARM                    DSMSMS
019600980304     C                   PARM                    DSMSPG
019700980304     C                   PARM                    DSMSKE
019800980304     C                   PARM                    DSMSER
019900980304     C*
020000980304     C* IMPOSTA LA DS -TASTI PREMUTI-
020100980304     C                   CALL      'XKEY'
020200980304     C                   PARM                    DSKEY
020300980304     C*---
020400980304     C* VARIABILI NON RIFERITE AL DATA BASE
020500980304     C*---
020600980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
020700980304     C*---
020800980304     C* VARIABILI DI CONTROLLO
020900980304     C*---
021000980304     C                   MOVEL     '0'           FINL00            1
021100980304     C                   MOVEL     '0'           F03L00            1
021200980304     C                   MOVEL     '0'           F12L00            1
021300980304     C                   MOVEL     '0'           ERRL00            1
021400980304     C*
021500980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
021600140310     C                   MOVEL     'TRULORSR'    DSMSPG
021700140310     C                   MOVEL     'TRULORSR'    MSGQUE
021800980304     C                   MOVEL     *BLANKS       MSGKEY
021900980304     C                   MOVEL     *BLANKS       DSMSMS
022000980304     C*
022100980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
022200980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
022300980304     C                   EXSR      FUNGES
022400980304     C*
022500980304     C* IMPOSTA IL VIDEO
022600140929     c                   CLEAR                   TRULORfds
022700140929     c                   CLEAR                   TRULORfds
022800140929     c                   CLEAR                   vidfil
022900140929     c                   CLEAR                   vidlin
023000980304     C*
023100980304     C                   ENDSR
023200980304     C*------------------------------------------------------------------------*
023300980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
023400980304     C*------------------------------------------------------------------------*
023500980304     C     FUNGES        BEGSR
023600980304     C*
023700980304     C* PULISCO CAMPI DI LAVORO
023800980304     C                   Z-ADD     0             W                 5 0
023900980304     C*
024000980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
024100980304     C* E CARICO TABELLA FUNZIONI ATTIVE
024200980304     C     1             DO        24            I
024300980304     C                   MOVEL     FUD(I)        FUDRIG
024400980304     C*
024500980304     C* PULIZIA FUNZIONI PRECEDENTI
024600980304     C                   MOVEL     *BLANKS       FUV(I)
024700980304     C*
024800980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
024900980304     C* (SALTA F24)
025000980304     C     FUDATT        IFEQ      'S'
025100980304     C     FUDFUN        ANDNE     '24'
025200980304     C                   ADD       1             W
025300980304     C                   MOVEL     FUDDES        FUV(W)
025400980304     C                   END
025500980304     C                   END
025600980304     C*
025700980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
025800980304     C* ALTRIMENTI DISATTIVO F24
025900980304     C     1             DO        24            I
026000980304     C                   MOVEL     FUD(I)        FUDRIG
026100980304     C*
026200980304     C     FUDFUN        IFEQ      '24'
026300980304     C     W             IFGT      6
026400980304     C                   MOVEL     'S'           FUDATT
026500980304     C                   ELSE
026600980304     C                   MOVEL     *BLANKS       FUDATT
026700980304     C                   END
026800980304     C                   MOVEL     FUDRIG        FUD(I)
026900980304     C                   END
027000980304     C                   END
027100980304     C*
027200980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
027300980304     C     W             DIV       6             I
027400980304     C                   MVR                     Z
027500980304     C*
027600980304     C     Z             IFNE      0
027700980304     C     I             ADD       1             Z
027800980304     C                   ELSE
027900980304     C                   Z-ADD     I             Z                 5 0
028000980304     C                   END
028100980304     C*
028200980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
028300980304     C                   ADD       1             FUNGRU
028400980304     C     FUNGRU        IFGT      Z
028500980304     C     FUNGRU        ORLT      1
028600980304     C                   Z-ADD     1             FUNGRU
028700980304     C                   END
028800980304     C*
028900980304     C* PRIMO GRUPPO DI 6 FUNZIONI
029000980304     C     FUNGRU        IFEQ      1
029100980304     C                   MOVEL     FUV(1)        FUN001
029200980304     C                   MOVEL     FUV(2)        FUN002
029300980304     C                   MOVEL     FUV(3)        FUN003
029400980304     C                   MOVEL     FUV(4)        FUN004
029500980304     C                   MOVEL     FUV(5)        FUN005
029600980304     C                   MOVEL     FUV(6)        FUN006
029700980304     C                   END
029800980304     C*
029900980304     C* SECONDO GRUPPO DI 6 FUNZIONI
030000980304     C     FUNGRU        IFEQ      2
030100980304     C                   MOVEL     FUV(7)        FUN001
030200980304     C                   MOVEL     FUV(8)        FUN002
030300980304     C                   MOVEL     FUV(9)        FUN003
030400980304     C                   MOVEL     FUV(10)       FUN004
030500980304     C                   MOVEL     FUV(11)       FUN005
030600980304     C                   MOVEL     FUV(12)       FUN006
030700980304     C                   END
030800980304     C*
030900980304     C* TERZO GRUPPO DI 6 FUNZIONI
031000980304     C     FUNGRU        IFEQ      3
031100980304     C                   MOVEL     FUV(13)       FUN001
031200980304     C                   MOVEL     FUV(14)       FUN002
031300980304     C                   MOVEL     FUV(15)       FUN003
031400980304     C                   MOVEL     FUV(16)       FUN004
031500980304     C                   MOVEL     FUV(17)       FUN005
031600980304     C                   MOVEL     FUV(18)       FUN006
031700980304     C                   END
031800980304     C*
031900980304     C* QUARTO GRUPPO DI 6 FUNZIONI
032000980304     C     FUNGRU        IFEQ      4
032100980304     C                   MOVEL     FUV(19)       FUN001
032200980304     C                   MOVEL     FUV(20)       FUN002
032300980304     C                   MOVEL     FUV(21)       FUN003
032400980304     C                   MOVEL     FUV(22)       FUN004
032500980304     C                   MOVEL     *BLANKS       FUN005
032600980304     C                   MOVEL     *BLANKS       FUN006
032700980304     C                   END
032800980304     C*
032900980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
033000980304     C                   MOVEL     *BLANKS       FUN007
033100980304     C                   MOVEL     *BLANKS       FUN008
033200980304     C*
033300980304     C     1             DO        24            I
033400980304     C                   MOVEL     FUD(I)        FUDRIG
033500980304     C     FUDFUN        IFEQ      '24'
033600980304     C     FUDATT        ANDEQ     'S'
033700980304     C                   MOVEL     FUDDES        FUN008
033800980304     C                   END
033900980304     C                   END
034000980304     C*
034100980304     C                   ENDSR
034200980304     C*------------------------------------------------------------------------*
034300980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
034400980304     C*------------------------------------------------------------------------*
034500980304     C     FUNCON        BEGSR
034600980304     C*
034700980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
034800980304     C                   EXSR      FUNVAR
034900980304     C*
035000980304     C* PULISCO CAMPI DI LAVORO
035100980304     C                   Z-ADD     0             I                 5 0
035200980304     C                   Z-ADD     0             W                 5 0
035300980304     C*
035400980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
035500980304     C     FUNKEY        IFNE      *BLANKS
035600980304     C     1             DO        24            I
035700980304     C                   MOVEL     FUD(I)        FUDRIG
035800980304     C*
035900980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
036000980304     C     FUNKEY        IFEQ      FUDFUN
036100980304     C     FUDATT        ANDEQ     'S'
036200980304     C                   Z-ADD     I             W
036300980304     C                   END
036400980304     C                   END
036500980304     C*
036600980304     C* FUNZIONE NON ATTIVA (ERRORE)
036700980304     C     W             IFEQ      0
036800980304     C*
036900980304     C* EMISSIONE MESSAGGIO DI ERRORE
037000980304     C                   MOVEL     MSG(1)        DSMSMS
037100980304     C*
037200980304     C                   END
037300980304     C                   END
037400980304     C*
037500980304     C* FUNZIONE ATTIVA (PROCEDI ...)
037600980304     C     W             IFNE      0
037700980304     C     FUNKEY        OREQ      *BLANKS
037800980304     C     FUNKEY        IFEQ      *BLANKS
037900980304     C                   EXSR      ENTGES
038000980304     C                   END
038100980304     C     FUNKEY        IFEQ      '03'
038200980304     C                   EXSR      F03GES
038300980304     C                   END
038400140317     C     FUNKEY        IFEQ      '10'
038500140317     C                   EXSR      F10GES
038600980304     C                   END
038700140317     C     FUNKEY        IFEQ      '12'
038800140317     C                   EXSR      F12GES
038900140317     C                   END
039000980304     C                   END
039100980304     C*
039200980304     C                   ENDSR
039300980304     C*------------------------------------------------------------------------*
039400980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
039500980304     C*------------------------------------------------------------------------*
039600980304     C     AZZVID        BEGSR
039700980304     C*
039800980304     C                   ENDSR
039900980304     C*------------------------------------------------------------------------*
040000980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
040100980304     C*------------------------------------------------------------------------*
040200980304     C     FUNVAR        BEGSR
040300980304     C*
040400980304     C* INIZIALIZZA VARIABILE -FUNKEY-
040500980304     C*
040600980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
040700980304     C     KEY           IFEQ      ENTER
040800980304     C                   MOVEL     *BLANKS       FUNKEY
040900980304     C                   END
041000980304     C     KEY           IFEQ      F1
041100980304     C                   MOVEL     '01'          FUNKEY
041200980304     C                   END
041300980304     C     KEY           IFEQ      F2
041400980304     C                   MOVEL     '02'          FUNKEY
041500980304     C                   END
041600980304     C     KEY           IFEQ      F3
041700980304     C                   MOVEL     '03'          FUNKEY
041800980304     C                   END
041900980304     C     KEY           IFEQ      F4
042000980304     C                   MOVEL     '04'          FUNKEY
042100980304     C                   END
042200980304     C     KEY           IFEQ      F5
042300980304     C                   MOVEL     '05'          FUNKEY
042400980304     C                   END
042500980304     C     KEY           IFEQ      F6
042600980304     C                   MOVEL     '06'          FUNKEY
042700980304     C                   END
042800980304     C     KEY           IFEQ      F7
042900980304     C                   MOVEL     '07'          FUNKEY
043000980304     C                   END
043100980304     C     KEY           IFEQ      F8
043200980304     C                   MOVEL     '08'          FUNKEY
043300980304     C                   END
043400980304     C     KEY           IFEQ      F9
043500980304     C                   MOVEL     '09'          FUNKEY
043600980304     C                   END
043700980304     C     KEY           IFEQ      F10
043800980304     C                   MOVEL     '10'          FUNKEY
043900980304     C                   END
044000980304     C     KEY           IFEQ      F11
044100980304     C                   MOVEL     '11'          FUNKEY
044200980304     C                   END
044300980304     C     KEY           IFEQ      F12
044400980304     C                   MOVEL     '12'          FUNKEY
044500980304     C                   END
044600980304     C     KEY           IFEQ      F13
044700980304     C                   MOVEL     '13'          FUNKEY
044800980304     C                   END
044900980304     C     KEY           IFEQ      F14
045000980304     C                   MOVEL     '14'          FUNKEY
045100980304     C                   END
045200980304     C     KEY           IFEQ      F15
045300980304     C                   MOVEL     '15'          FUNKEY
045400980304     C                   END
045500980304     C     KEY           IFEQ      F16
045600980304     C                   MOVEL     '16'          FUNKEY
045700980304     C                   END
045800980304     C     KEY           IFEQ      F17
045900980304     C                   MOVEL     '17'          FUNKEY
046000980304     C                   END
046100980304     C     KEY           IFEQ      F18
046200980304     C                   MOVEL     '18'          FUNKEY
046300980304     C                   END
046400980304     C     KEY           IFEQ      F19
046500980304     C                   MOVEL     '19'          FUNKEY
046600980304     C                   END
046700980304     C     KEY           IFEQ      F20
046800980304     C                   MOVEL     '20'          FUNKEY
046900980304     C                   END
047000980304     C     KEY           IFEQ      F21
047100980304     C                   MOVEL     '21'          FUNKEY
047200980304     C                   END
047300980304     C     KEY           IFEQ      F22
047400980304     C                   MOVEL     '22'          FUNKEY
047500980304     C                   END
047600980304     C     KEY           IFEQ      F23
047700980304     C                   MOVEL     '23'          FUNKEY
047800980304     C                   END
047900980304     C     KEY           IFEQ      F24
048000980304     C                   MOVEL     '24'          FUNKEY
048100980304     C                   END
048200980304     C     KEY           IFEQ      ROLLUP
048300980304     C                   MOVEL     'RU'          FUNKEY
048400980304     C                   END
048500980304     C     KEY           IFEQ      HELP
048600980304     C                   MOVEL     'HP'          FUNKEY
048700980304     C                   END
048800980304     C                   ENDSR
048900980304     C*------------------------------------------------------------------------*
049000980304     C* NOTE COSTRUZIONE SCHIERE
049100980304     C*------------------------------------------------------------------------*
049200980304     C*
049300980304     C* -MSG-MESSAGGI
049400980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
049500980304     C* (NON TOCCARE MSG,1 E MSG,2)
049600980304     C*
049700980304     C* -CMD-COMANDI
049800980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
049900980304     C*
050000980304     C* -OPD-OPZIONI
050100980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
050200980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
050300980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
050400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
050500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
050600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
050700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
050800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
050900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
051000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
051100980304     C*
051200980304     C* -FUD-FUNZIONI
051300980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
051400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
051500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
051600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
051700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
051800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
051900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
052000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
052100980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
052200980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
052300980304     o*-------------------------------------------------------------------------
052400980304** msg - messaggi
052500140317Funzione non attiva
052600140317Immettere per consegne o per ritiri
052700140317CAP o località errati
052800140210fnlrj6R- 4
052900140210fnlrj6R- 5
053000140210fnlrj6R- 6
053100140210fnlrj6R- 7
053200140210LRLRj6R- 8
053300140210LRLRj6R- 9
053400140210LRLRj6R- 10
053500980304** FUD - FUNZIONI
053600980304                   -        (S/N)=N                        funz.01¬
053700980304                   -        (S/N)=N                        funz.02¬
053800980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
053900980304                   -        (S/N)=N                        funz.04¬
054000980304                   -        (S/N)=N                        funz.05¬
054100980304                   -        (S/N)=N                        funz.06¬
054200980304                   -        (S/N)=N                        funz.07¬
054300980304                   -        (S/N)=N                        funz.08¬
054400980304                   -        (S/N)=N                        funz.09¬
054500140317F10=Pulisce        -        (S/N)=S                        funz.10¬
054600980304                   -        (S/N)=N                        funz.11¬
054700980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
054800980304                   -        (S/N)=N                        funz.13¬
054900980304                   -        (S/N)=N                        funz.14¬
055000980304                   -        (S/N)=N                        funz.15¬
055100980304                   -        (S/N)=N                        funz.16¬
055200980304                   -        (S/N)=N                        funz.17¬
055300980304                   -        (S/N)=N                        funz.18¬
055400980304                   -        (S/N)=N                        funz.19¬
055500980304                   -        (S/N)=N                        funz.20¬
055600980304                   -        (S/N)=N                        funz.21¬
055700980304                   -        (S/N)=N                        funz.22¬
055800980304                   -        (S/N)=N                        funz.23¬
055900980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

000100080612     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('TEST_FG')
000200980304     H*------------------------------------------------------------------------*
000300081020     H* UBCHKLGN - TEST
000400980304     H*------------------------------------------------------------------------*
000500980304     H DECEDIT('0,') DATEDIT(*DMY.)
000600980304     F*------------------------------------------------------------------------*
000700980304     F* DATA BASE
000800980304     F*------------------------------------------------------------------------*
000900081020     FUBCHKLGNTDCF   E             WORKSTN INFDS(DEVDS1)
001000081020     F                                     SFILE(UBCLDMS:MSGULT)
001100980304     D*------------------------------------------------------------------------*
001200980304     D* SCHIERE
001300980304     D*------------------------------------------------------------------------*
001400980304     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
001500980304     D*------------
001600980304     D* DESCRIZIONI TASTI FUNZIONALI
001700980304     D*------------
001800980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001900980304     D*------------
002000980304     D* FUNZIONI DA CARICARE A VIDEO
002100980304     D*------------
002200980304     D FUV             S             19    DIM(24)
002300980304     D*------------------------------------------------------------------------*
002400980304     D* INPUT
002500980304     D*------------------------------------------------------------------------*
002600980304     D*------------
002700980304     D* RIDEFINIZIONE FUNZIONI VIDEO
002800980304     D*------------
002900980304     D FUDDS           DS
003000980304     D  FUDDE1                 1      1
003100980304     D  FUDFUN                 2      3
003200980304     D  FUDDE2                 4     19
003300980304     D  FUDFI0                20     34
003400980304     D  FUDATT                35     35
003500980304     D  FUDFI1                36     80
003600980304     D  FUDDES                 1     19
003700980304     D  FUDRIG                 1     80
003800980304     D*------------
003900980304     D* DS GESTIONE TASTI FUNZIONALI
004000980304     D*------------
004100980304     D DSKEY         E DS
004200980304     D DEVDS1          DS
004300980304     D  KEY                  369    369
004400980304     D*------------
004500980304     D* DS "XMSG" - GESTIONE MESSAGGI
004600980304     D*------------
004700980304     D DSMSG         E DS
004800980304     D*------------------
004900980304     D* ARCHITETTURA
005000980304     D*------------------
005100980304     D KPJBA         E DS
005200080612     D*------------------
005300081020     D* PASSAGGIO PARAMETRI A PROCEDURE UBCHKLGN
005400080612     D*------------------
005500081020     D InUser          S             10A
005600081020     D InPassword      S             10A
005700081020     D OutUsrHdl       S             12A
005800081020     D OutLgnSts       S             10A
005900080612     D*------------------
006000080612     D* LINKING A DEFINIZIONI ESTERNE
006100080612     D*------------------
006200081020     D/COPY GAITRASRC/SRCPROTOPR,UBCHKLGN
006300980304     C*------------------------------------------------------------------------*
006400980304     C* MAIN LINES
006500980304     C*------------------------------------------------------------------------*
006600980304     C*
006700980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006800980304DO  1C     F03L00        DOUEQ     '1'
006900980304     C     F12L00        OREQ      '1'
007000980304     C     FINL00        OREQ      '1'
007100980304     C*
007200980304     C* EMETTE FORMATO VIDEO
007300980304     C                   EXSR      BUTFMR
007400980304     C*
007500980304     C* GESTIONE TASTI FUNZIONALI
007600980304     C                   EXSR      FUNCON
007700980304E   1C                   ENDDO
007800980304     C*
007900980304     C                   SETON                                        LR
008000980304     C*------------------------------------------------------------------------*
008100980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
008200980304     C*------------------------------------------------------------------------*
008300980304     C     BUTFMR        BEGSR
008400980304     C*
008500980304     C* EMETTE LA TESTATA
008600081020     C                   WRITE     UBCLD01
008700980304     C*
008800980304     C* SPEDISCE EVENTUALE MESSAGGIO
008900980304     C                   EXSR      SNDMSG
009000980304     C*
009100980304     C* EMETTE LE FUNZIONI
009200081020     C                   WRITE     UBCLD02
009300980304     C*
009400980304     C* EMETTE IL VIDEO
009500081020     C                   EXFMT     UBCLD03
009600980304     C*
009700980304     C* REIMPOSTA FLAG DI ERRORE
009800980304     C                   MOVEL     '0'           ERRL00
009900980304     C*
010000980304     C                   ENDSR
010100980304     C*------------------------------------------------------------------------*
010200980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010300980304     C*------------------------------------------------------------------------*
010400980304     C     SNDMSG        BEGSR
010500980304     C*
010600980304     C* PULISCE IL SFILE MESSAGGI
010700980304     C                   MOVEL     '1'           *IN40
010800081020     C                   WRITE     UBCLDMC
010900980304     C                   MOVEL     '0'           *IN40
011000980304     C*
011100980304     C* VISUALIZZA IL MESSAGGIO
011200980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
011300980304     C                   CALL      'XMSG'        XMSGPA
011400980304IF  1C     DSMSER        IFEQ      *ZEROS
011500980304     C                   MOVEL     DSMSKE        MSGKEY
011600980304E   1C                   ENDIF
011700980304     C                   Z-ADD     1             MSGULT
011800081020     C                   WRITE     UBCLDMS
011900081020     C                   WRITE     UBCLDMC
012000980304     C*
012100980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
012200980304     C                   MOVEL     *BLANKS       DSMSMS
012300980304     C*
012400980304     C                   ENDSR
012500980304     C*------------------------------------------------------------------------*
012600980304     C* ENTGES - GESTIONE ENTER -> INVIO
012700980304     C*------------------------------------------------------------------------*
012800980304     C     ENTGES        BEGSR
012900080612     C*
013000081020     C                   eval      InUser     = VIDUSR
013100081020     C                   eval      InPassword = VIDPWD
013200080612     C*
013300081020     C* Chiamata metodo Check
013400081020     C                   eval      VIDRTNCOD=%char(UBCHKLGN_Check(
013500081020     C                                                InUser
013600081020     C                                               :InPassword
013700081020     C                                               :OutUsrHdl
013800081020     C                                               :OutLgnSts
013900081020     C                                               ))
014000081020     C                   eval      VIDLGNSTS = OutLgnSts
014100080612     C*
014200980304     C                   ENDSR
014300980304     C*------------------------------------------------------------------------*
014400980304     C* F03GES - GESTIONE F03 -> FINE
014500980304     C*------------------------------------------------------------------------*
014600980304     C     F03GES        BEGSR
014700980304     C*
014800980304     C* IMPOSTA "PREMUTO F03"
014900980304     C                   MOVEL     '1'           F03L00
015000980304     C*
015100980304     C                   ENDSR
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
020200081020     C                   MOVEL     'UBCHKLGN'    DSMSPG
020300081020     C                   MOVEL     'UBCHKLGN'    MSGQUE
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
036400980304     C     FUNKEY        IFEQ      '12'
036500980304     C                   EXSR      F12GES
036600980304     C                   END
036700980304     C                   END
036800980304     C*
036900980304     C                   ENDSR
037000980304     C*------------------------------------------------------------------------*
037100980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
037200980304     C*------------------------------------------------------------------------*
037300980304     C     AZZVID        BEGSR
037400980304     C*
037500980304     C                   ENDSR
037600980304     C*------------------------------------------------------------------------*
037700980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
037800980304     C*------------------------------------------------------------------------*
037900980304     C     FUNVAR        BEGSR
038000980304     C*
038100980304     C* INIZIALIZZA VARIABILE -FUNKEY-
038200980304     C*
038300980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
038400980304     C     KEY           IFEQ      ENTER
038500980304     C                   MOVEL     *BLANKS       FUNKEY
038600980304     C                   END
038700980304     C     KEY           IFEQ      F1
038800980304     C                   MOVEL     '01'          FUNKEY
038900980304     C                   END
039000980304     C     KEY           IFEQ      F2
039100980304     C                   MOVEL     '02'          FUNKEY
039200980304     C                   END
039300980304     C     KEY           IFEQ      F3
039400980304     C                   MOVEL     '03'          FUNKEY
039500980304     C                   END
039600980304     C     KEY           IFEQ      F4
039700980304     C                   MOVEL     '04'          FUNKEY
039800980304     C                   END
039900980304     C     KEY           IFEQ      F5
040000980304     C                   MOVEL     '05'          FUNKEY
040100980304     C                   END
040200980304     C     KEY           IFEQ      F6
040300980304     C                   MOVEL     '06'          FUNKEY
040400980304     C                   END
040500980304     C     KEY           IFEQ      F7
040600980304     C                   MOVEL     '07'          FUNKEY
040700980304     C                   END
040800980304     C     KEY           IFEQ      F8
040900980304     C                   MOVEL     '08'          FUNKEY
041000980304     C                   END
041100980304     C     KEY           IFEQ      F9
041200980304     C                   MOVEL     '09'          FUNKEY
041300980304     C                   END
041400980304     C     KEY           IFEQ      F10
041500980304     C                   MOVEL     '10'          FUNKEY
041600980304     C                   END
041700980304     C     KEY           IFEQ      F11
041800980304     C                   MOVEL     '11'          FUNKEY
041900980304     C                   END
042000980304     C     KEY           IFEQ      F12
042100980304     C                   MOVEL     '12'          FUNKEY
042200980304     C                   END
042300980304     C     KEY           IFEQ      F13
042400980304     C                   MOVEL     '13'          FUNKEY
042500980304     C                   END
042600980304     C     KEY           IFEQ      F14
042700980304     C                   MOVEL     '14'          FUNKEY
042800980304     C                   END
042900980304     C     KEY           IFEQ      F15
043000980304     C                   MOVEL     '15'          FUNKEY
043100980304     C                   END
043200980304     C     KEY           IFEQ      F16
043300980304     C                   MOVEL     '16'          FUNKEY
043400980304     C                   END
043500980304     C     KEY           IFEQ      F17
043600980304     C                   MOVEL     '17'          FUNKEY
043700980304     C                   END
043800980304     C     KEY           IFEQ      F18
043900980304     C                   MOVEL     '18'          FUNKEY
044000980304     C                   END
044100980304     C     KEY           IFEQ      F19
044200980304     C                   MOVEL     '19'          FUNKEY
044300980304     C                   END
044400980304     C     KEY           IFEQ      F20
044500980304     C                   MOVEL     '20'          FUNKEY
044600980304     C                   END
044700980304     C     KEY           IFEQ      F21
044800980304     C                   MOVEL     '21'          FUNKEY
044900980304     C                   END
045000980304     C     KEY           IFEQ      F22
045100980304     C                   MOVEL     '22'          FUNKEY
045200980304     C                   END
045300980304     C     KEY           IFEQ      F23
045400980304     C                   MOVEL     '23'          FUNKEY
045500980304     C                   END
045600980304     C     KEY           IFEQ      F24
045700980304     C                   MOVEL     '24'          FUNKEY
045800980304     C                   END
045900980304     C     KEY           IFEQ      ROLLUP
046000980304     C                   MOVEL     'RU'          FUNKEY
046100980304     C                   END
046200980304     C     KEY           IFEQ      HELP
046300980304     C                   MOVEL     'HP'          FUNKEY
046400980304     C                   END
046500980304     C                   ENDSR
046600980304     C*------------------------------------------------------------------------*
046700980304     C* NOTE COSTRUZIONE SCHIERE
046800980304     C*------------------------------------------------------------------------*
046900980304     C*
047000980304     C* -MSG-MESSAGGI
047100980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
047200980304     C* (NON TOCCARE MSG,1 E MSG,2)
047300980304     C*
047400980304     C* -CMD-COMANDI
047500980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
047600980304     C*
047700980304     C* -OPD-OPZIONI
047800980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
047900980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
048000980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
048100980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
048200980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
048300980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
048400980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048500980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048600980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
048700980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
048800980304     C*
048900980304     C* -FUD-FUNZIONI
049000980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
049100980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
049200980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
049300980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
049400980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049500980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049600980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049700980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
049800980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
049900980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
050000980304     o*-------------------------------------------------------------------------
050100980304** msg - messaggi
050200081020UBCHKLGN Funzione non attiva
050300081020UBCHKLGN 2
050400081020UBCHKLGN 3
050500081020UBCHKLGN 4
050600081020UBCHKLGN 5
050700081020UBCHKLGN 6
050800081020UBCHKLGN 7
050900081020UBCHKLGN 8
051000081020UBCHKLGN 9
051100081020UBCHKLGN 10
051200980304** FUD - FUNZIONI
051300980304                   -        (S/N)=N                        funz.01¬
051400980304                   -        (S/N)=N                        funz.02¬
051500980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
051600980304                   -        (S/N)=N                        funz.04¬
051700980304                   -        (S/N)=N                        funz.05¬
051800980304                   -        (S/N)=N                        funz.06¬
051900980304                   -        (S/N)=N                        funz.07¬
052000980304                   -        (S/N)=N                        funz.08¬
052100980304                   -        (S/N)=N                        funz.09¬
052200980304                   -        (S/N)=N                        funz.10¬
052300980304                   -        (S/N)=N                        funz.11¬
052400980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
052500980304                   -        (S/N)=N                        funz.13¬
052600980304                   -        (S/N)=N                        funz.14¬
052700980304                   -        (S/N)=N                        funz.15¬
052800980304                   -        (S/N)=N                        funz.16¬
052900980304                   -        (S/N)=N                        funz.17¬
053000980304                   -        (S/N)=N                        funz.18¬
053100980304                   -        (S/N)=N                        funz.19¬
053200980304                   -        (S/N)=N                        funz.20¬
053300980304                   -        (S/N)=N                        funz.21¬
053400980304                   -        (S/N)=N                        funz.22¬
053500980304                   -        (S/N)=N                        funz.23¬
053600980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

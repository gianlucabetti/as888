000100980304     H*------------------------------------------------------------------------*
000200991018     H* clienti unificanti    - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800991018     FTIBS11D   CF   E             WORKSTN INFDS(DEVDS1)
000900991018     F                                     SFILE(BS11DMS:MSGULT)
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
005000991018     d tibs10        E DS                  EXTNAME(tibs10ds)
005100991018     d wcampo          ds           330
005200991018     d  cod001
005300991018     d  cod002
005400991018     d  cod003
005500991018     d  cod004
005600991018     d  cod005
005700991018     d  cod006
005800991018     d  cod007
005900991018     d  cod008
006000991018     d  cod009
006100991018     d  cod010
006200991018     d  cod011
006300991018     d  cod012
006400991018     d  cod013
006500991018     d  cod014
006600991018     d  cod015
006700991018     d  cod016
006800991018     d  cod017
006900991018     d  cod018
007000991018     d  cod019
007100991018     d  cod020
007200991018     d  cod021
007300991018     d  cod022
007400991018     d  cod023
007500991018     d  cod024
007600991018     d  cod025
007700991018     d  cod026
007800991018     d  cod027
007900991018     d  cod028
008000991018     d  cod029
008100991018     d  cod030
008200991018     D*------------------
008300991018     D* DS "XSRDA8" - CONTROLLA DATA (8)
008400991018     D*------------------
008500991018     D WLBDA8          DS                  INZ
008600991018     D  G08DAT                 1      8  0
008700991018     D  G08INV                 9     16  0
008800991018     D  G08ERR                17     17
008900991018     D  G08TGI                18     22  0
009000980304     D*------------------
009100980304     D* ARCHITETTURA
009200980304     D*------------------
009300980304     D KPJBA         E DS
009400980304     C*------------------------------------------------------------------------*
009500980304     C* MAIN LINES
009600980304     C*------------------------------------------------------------------------*
009700980304     C*
009800980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009900980304DO  1C     F03L00        DOUEQ     '1'
010000980304     C     F12L00        OREQ      '1'
010100980304     C     FINL00        OREQ      '1'
010200980304     C*
010300980304     C* EMETTE FORMATO VIDEO
010400980304     C                   EXSR      BUTFMR
010500980304     C*
010600980304     C* GESTIONE TASTI FUNZIONALI
010700980304     C                   EXSR      FUNCON
010800980304E   1C                   ENDDO
010900980304     C*
011000980304     C                   SETON                                        LR
011100980304     C*------------------------------------------------------------------------*
011200980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011300980304     C*------------------------------------------------------------------------*
011400980304     C     BUTFMR        BEGSR
011500980304     C*
011600980304     C* EMETTE LA TESTATA
011700991018     C                   WRITE     BS11D01
011800980304     C*
011900980304     C* SPEDISCE EVENTUALE MESSAGGIO
012000980304     C                   EXSR      SNDMSG
012100980304     C*
012200980304     C* EMETTE LE FUNZIONI
012300991018     C                   WRITE     BS11D02
012400980304     C*
012500980304     C* EMETTE IL VIDEO
012600991018     C                   EXFMT     BS11D03
012700980304     C*
012800980304     C* REIMPOSTA FLAG DI ERRORE
012900980304     C                   MOVEL     '0'           ERRL00
013000980304     C*
013100980304     C* REIMPOSTA INDICATORI DI ERRORE
013200980304     C                   SETOFF                                       59
013300980304     C                   SETOFF                                       606162
013400980304     C                   SETOFF                                       636465
013500980304     C                   SETOFF                                       666768
013600980304     C                   SETOFF                                       697071
013700980304     C                   SETOFF                                       727374
013800980304     C                   SETOFF                                       757677
013900980304     C                   SETOFF                                       787980
014000980304     C                   SETOFF                                       818283
014100980304     C                   SETOFF                                       848586
014200980304     C                   SETOFF                                       878889
014300980304     C*
014400980304     C                   ENDSR
014500980304     C*------------------------------------------------------------------------*
014600980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014700980304     C*------------------------------------------------------------------------*
014800980304     C     SNDMSG        BEGSR
014900980304     C*
015000980304     C* PULISCE IL SFILE MESSAGGI
015100980304     C                   MOVEL     '1'           *IN40
015200991018     C                   WRITE     BS11DMC
015300980304     C                   MOVEL     '0'           *IN40
015400980304     C*
015500980304     C* VISUALIZZA IL MESSAGGIO
015600980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
015700980304     C                   CALL      'XMSG'        XMSGPA
015800980304IF  1C     DSMSER        IFEQ      *ZEROS
015900980304     C                   MOVEL     DSMSKE        MSGKEY
016000980304E   1C                   ENDIF
016100980304     C                   Z-ADD     1             MSGULT
016200991018     C                   WRITE     BS11DMS
016300991018     C                   WRITE     BS11DMC
016400980304     C*
016500980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016600980304     C                   MOVEL     *BLANKS       DSMSMS
016700980304     C*
016800980304     C                   ENDSR
016900980304     C*------------------------------------------------------------------------*
017000980304     C* ENTGES - GESTIONE ENTER -> INVIO
017100980304     C*------------------------------------------------------------------------*
017200980304     C     ENTGES        BEGSR
017300980304     C*
017400991018     c                   CALL      'TIBS10R'
017500991018     c                   PARM                    tibs10
017600991018     c                   movel     d10skc        wcampo
017700980304     C                   ENDSR
017800980304     C*------------------------------------------------------------------------*
017900980304     C* F03GES - GESTIONE F03 -> FINE
018000980304     C*------------------------------------------------------------------------*
018100980304     C     F03GES        BEGSR
018200980304     C*
018300980304     C* IMPOSTA "PREMUTO F03"
018400980304     C                   MOVEL     '1'           F03L00
018500980304     C*
018600980304     C                   ENDSR
018700980304     C*------------------------------------------------------------------------*
018800980304     C* F12GES - GESTIONE F12 -> RITORNO
018900980304     C*------------------------------------------------------------------------*
019000980304     C     F12GES        BEGSR
019100980304     C*
019200980304     C* IMPOSTA "PREMUTO F12"
019300980304     C                   MOVEL     '1'           F12L00
019400980304     C*
019500980304     C                   ENDSR
019600980304     C*------------------------------------------------------------------------*
019700980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019800980304     C*------------------------------------------------------------------------*
019900980304     C     F24GES        BEGSR
020000980304     C*
020100980304     C                   EXSR      FUNGES
020200980304     C*
020300980304     C                   ENDSR
020400980304     C*------------------------------------------------------------------------*
020500980304     C* *INZSR - OPERAZIONI INIZIALI
020600980304     C*------------------------------------------------------------------------*
020700980304     C     *INZSR        BEGSR
020800980304     C*---
020900980304     C* RICEVIMENTO PARAMETRI
021000980304     C*---
021100980304     C     *ENTRY        PLIST
021200980304     C                   PARM                    KPJBA
021300980304     C*
021400980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
021500980304     C     XMSGPA        PLIST
021600980304     C                   PARM                    DSMSMS
021700980304     C                   PARM                    DSMSPG
021800980304     C                   PARM                    DSMSKE
021900980304     C                   PARM                    DSMSER
022000980304     C*
022100980304     C* IMPOSTA LA DS -TASTI PREMUTI-
022200980304     C                   CALL      'XKEY'
022300980304     C                   PARM                    DSKEY
022400980304     C*---
022500980304     C* VARIABILI NON RIFERITE AL DATA BASE
022600980304     C*---
022700980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
022800980304     C*---
022900980304     C* VARIABILI DI CONTROLLO
023000980304     C*---
023100980304     C                   MOVEL     '0'           FINL00            1
023200980304     C                   MOVEL     '0'           F03L00            1
023300980304     C                   MOVEL     '0'           F12L00            1
023400980304     C                   MOVEL     '0'           ERRL00            1
023500980304     C*
023600980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
023700991018     C                   MOVEL     'TIBS11R'     DSMSPG
023800991018     C                   MOVEL     'TIBS11R'     MSGQUE
023900980304     C                   MOVEL     *BLANKS       MSGKEY
024000980304     C                   MOVEL     *BLANKS       DSMSMS
024100980304     C*
024200980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
024300980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
024400980304     C                   EXSR      FUNGES
024500980304     C*
024600980304     C* IMPOSTA IL VIDEO
024700991018     c                   CLEAR                   tibs10
024800991018     c                   move      *zeros        wcampo
024900991018     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
025000991018     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
025100991018     C                   Z-ADD     WN8           G08DAT
025200991018     C                   Z-ADD     *ZEROS        G08INV
025300991018     C                   MOVEL     '0'           G08ERR
025400991018     C                   CALL      'XSRDA8'
025500991018     C                   PARM                    WLBDA8
025600991018     C                   Z-ADD     G08INV        D10drf                         *DATA CORRENTE AA/M/
025700980304     C*
025800980304     C                   ENDSR
025900980304     C*------------------------------------------------------------------------*
026000980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
026100980304     C*------------------------------------------------------------------------*
026200980304     C     FUNGES        BEGSR
026300980304     C*
026400980304     C* PULISCO CAMPI DI LAVORO
026500980304     C                   Z-ADD     0             W                 5 0
026600980304     C*
026700980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
026800980304     C* E CARICO TABELLA FUNZIONI ATTIVE
026900980304     C     1             DO        24            I
027000980304     C                   MOVEL     FUD(I)        FUDRIG
027100980304     C*
027200980304     C* PULIZIA FUNZIONI PRECEDENTI
027300980304     C                   MOVEL     *BLANKS       FUV(I)
027400980304     C*
027500980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
027600980304     C* (SALTA F24)
027700980304     C     FUDATT        IFEQ      'S'
027800980304     C     FUDFUN        ANDNE     '24'
027900980304     C                   ADD       1             W
028000980304     C                   MOVEL     FUDDES        FUV(W)
028100980304     C                   END
028200980304     C                   END
028300980304     C*
028400980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
028500980304     C* ALTRIMENTI DISATTIVO F24
028600980304     C     1             DO        24            I
028700980304     C                   MOVEL     FUD(I)        FUDRIG
028800980304     C*
028900980304     C     FUDFUN        IFEQ      '24'
029000980304     C     W             IFGT      6
029100980304     C                   MOVEL     'S'           FUDATT
029200980304     C                   ELSE
029300980304     C                   MOVEL     *BLANKS       FUDATT
029400980304     C                   END
029500980304     C                   MOVEL     FUDRIG        FUD(I)
029600980304     C                   END
029700980304     C                   END
029800980304     C*
029900980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
030000980304     C     W             DIV       6             I
030100980304     C                   MVR                     Z
030200980304     C*
030300980304     C     Z             IFNE      0
030400980304     C     I             ADD       1             Z
030500980304     C                   ELSE
030600980304     C                   Z-ADD     I             Z                 5 0
030700980304     C                   END
030800980304     C*
030900980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
031000980304     C                   ADD       1             FUNGRU
031100980304     C     FUNGRU        IFGT      Z
031200980304     C     FUNGRU        ORLT      1
031300980304     C                   Z-ADD     1             FUNGRU
031400980304     C                   END
031500980304     C*
031600980304     C* PRIMO GRUPPO DI 6 FUNZIONI
031700980304     C     FUNGRU        IFEQ      1
031800980304     C                   MOVEL     FUV(1)        FUN001
031900980304     C                   MOVEL     FUV(2)        FUN002
032000980304     C                   MOVEL     FUV(3)        FUN003
032100980304     C                   MOVEL     FUV(4)        FUN004
032200980304     C                   MOVEL     FUV(5)        FUN005
032300980304     C                   MOVEL     FUV(6)        FUN006
032400980304     C                   END
032500980304     C*
032600980304     C* SECONDO GRUPPO DI 6 FUNZIONI
032700980304     C     FUNGRU        IFEQ      2
032800980304     C                   MOVEL     FUV(7)        FUN001
032900980304     C                   MOVEL     FUV(8)        FUN002
033000980304     C                   MOVEL     FUV(9)        FUN003
033100980304     C                   MOVEL     FUV(10)       FUN004
033200980304     C                   MOVEL     FUV(11)       FUN005
033300980304     C                   MOVEL     FUV(12)       FUN006
033400980304     C                   END
033500980304     C*
033600980304     C* TERZO GRUPPO DI 6 FUNZIONI
033700980304     C     FUNGRU        IFEQ      3
033800980304     C                   MOVEL     FUV(13)       FUN001
033900980304     C                   MOVEL     FUV(14)       FUN002
034000980304     C                   MOVEL     FUV(15)       FUN003
034100980304     C                   MOVEL     FUV(16)       FUN004
034200980304     C                   MOVEL     FUV(17)       FUN005
034300980304     C                   MOVEL     FUV(18)       FUN006
034400980304     C                   END
034500980304     C*
034600980304     C* QUARTO GRUPPO DI 6 FUNZIONI
034700980304     C     FUNGRU        IFEQ      4
034800980304     C                   MOVEL     FUV(19)       FUN001
034900980304     C                   MOVEL     FUV(20)       FUN002
035000980304     C                   MOVEL     FUV(21)       FUN003
035100980304     C                   MOVEL     FUV(22)       FUN004
035200980304     C                   MOVEL     *BLANKS       FUN005
035300980304     C                   MOVEL     *BLANKS       FUN006
035400980304     C                   END
035500980304     C*
035600980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
035700980304     C                   MOVEL     *BLANKS       FUN007
035800980304     C                   MOVEL     *BLANKS       FUN008
035900980304     C*
036000980304     C     1             DO        24            I
036100980304     C                   MOVEL     FUD(I)        FUDRIG
036200980304     C     FUDFUN        IFEQ      '24'
036300980304     C     FUDATT        ANDEQ     'S'
036400980304     C                   MOVEL     FUDDES        FUN008
036500980304     C                   END
036600980304     C                   END
036700980304     C*
036800980304     C                   ENDSR
036900980304     C*------------------------------------------------------------------------*
037000980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
037100980304     C*------------------------------------------------------------------------*
037200980304     C     FUNCON        BEGSR
037300980304     C*
037400980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
037500980304     C                   EXSR      FUNVAR
037600980304     C*
037700980304     C* PULISCO CAMPI DI LAVORO
037800980304     C                   Z-ADD     0             I                 5 0
037900980304     C                   Z-ADD     0             W                 5 0
038000980304     C*
038100980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
038200980304     C     FUNKEY        IFNE      *BLANKS
038300980304     C     1             DO        24            I
038400980304     C                   MOVEL     FUD(I)        FUDRIG
038500980304     C*
038600980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
038700980304     C     FUNKEY        IFEQ      FUDFUN
038800980304     C     FUDATT        ANDEQ     'S'
038900980304     C                   Z-ADD     I             W
039000980304     C                   END
039100980304     C                   END
039200980304     C*
039300980304     C* FUNZIONE NON ATTIVA (ERRORE)
039400980304     C     W             IFEQ      0
039500980304     C*
039600980304     C* EMISSIONE MESSAGGIO DI ERRORE
039700980304     C                   MOVEL     MSG(1)        DSMSMS
039800980304     C*
039900980304     C                   END
040000980304     C                   END
040100980304     C*
040200980304     C* FUNZIONE ATTIVA (PROCEDI ...)
040300980304     C     W             IFNE      0
040400980304     C     FUNKEY        OREQ      *BLANKS
040500980304     C     FUNKEY        IFEQ      *BLANKS
040600980304     C                   EXSR      ENTGES
040700980304     C                   END
040800980304     C     FUNKEY        IFEQ      '03'
040900980304     C                   EXSR      F03GES
041000980304     C                   END
041100980304     C     FUNKEY        IFEQ      '12'
041200980304     C                   EXSR      F12GES
041300980304     C                   END
041400980304     C                   END
041500980304     C*
041600980304     C                   ENDSR
041700980304     C*------------------------------------------------------------------------*
041800980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
041900980304     C*------------------------------------------------------------------------*
042000980304     C     AZZVID        BEGSR
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
054900991018TIBS11R- Funzione non attiva
055000991018TIBS11R- 2
055100991018TIBS11R- 3
055200991018TIBS11R- 4
055300991018TIBS11R- 5
055400991018TIBS11R- 6
055500991018TIBS11R- 7
055600991018TIBS11R- 8
055700991018TIBS11R- 9
055800991018TIBS11R- 10
055900980304** FUD - FUNZIONI
056000980304                   -        (S/N)=N                        funz.01¬
056100980304                   -        (S/N)=N                        funz.02¬
056200980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
056300980304                   -        (S/N)=N                        funz.04¬
056400980304                   -        (S/N)=N                        funz.05¬
056500980304                   -        (S/N)=N                        funz.06¬
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

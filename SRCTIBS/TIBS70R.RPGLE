000100971217     H*------------------------------------------------------------------------*
000200971217     H* LETTURA ANAGRAFICHE CLIENTE - TEST
000300971217     H*------------------------------------------------------------------------*
000400971217     H DECEDIT('0,') DATEDIT(*DMY.)
000500971217     F*------------------------------------------------------------------------*
000600971217     F* DATA BASE
000700971217     F*------------------------------------------------------------------------*
000800971217     FTIBS70D   CF   E             WORKSTN INFDS(DEVDS1)
000900971217     F                                     SFILE(BS70DMS:MSGULT)
001000971217     D*------------------------------------------------------------------------*
001100971217     D* SCHIERE
001200971217     D*------------------------------------------------------------------------*
001300971217     d msg             S             80    DIM(10) CTDATA PERRCD(1)             *comandi clp
001400971217     D*------------
001500971217     D* DESCRIZIONI TASTI FUNZIONALI
001600971217     D*------------
001700971217     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001800971217     D*------------
001900971217     D* FUNZIONI DA CARICARE A VIDEO
002000971217     D*------------
002100971217     D FUV             S             19    DIM(24)
002200971217     D*------------------------------------------------------------------------*
002300971217     D* INPUT
002400971217     D*------------------------------------------------------------------------*
002500971217     D*------------
002600971217     D* RIDEFINIZIONE FUNZIONI VIDEO
002700971217     D*------------
002800971217     D FUDDS           DS
002900971217     D  FUDDE1                 1      1
003000971217     D  FUDFUN                 2      3
003100971217     D  FUDDE2                 4     19
003200971217     D  FUDFI0                20     34
003300971217     D  FUDATT                35     35
003400971217     D  FUDFI1                36     80
003500971217     D  FUDDES                 1     19
003600971217     D  FUDRIG                 1     80
003700971217     D*------------
003800971217     D* DS GESTIONE TASTI FUNZIONALI
003900971217     D*------------
004000971217     D DSKEY         E DS
004100971217     D DEVDS1          DS
004200971217     D  KEY                  369    369
004300971217     D*------------
004400971217     D* DS "XMSG" - GESTIONE MESSAGGI
004500971217     D*------------
004600971217     D DSMSG         E DS
004700971217     D*------------
004800971217     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900971217     D*------------------
005000971217     D WLBDA8          DS                  INZ
005100971217     D  G08DAT                 1      8  0
005200971217     D  G08INV                 9     16  0
005300971217     D  G08ERR                17     17
005400971217     D  G08TGI                18     22  0
005500971217     D*------------------
005600971217     D* ARCHITETTURA
005700971217     D*------------------
005800971217     d bs69ds        E DS                  EXTNAME(tibs69ds)                    *ds di procedura
005900971217     d cnacods       E DS                  EXTNAME(cnaco00f)                    *record cnaco
006000971217     d cnindds       E DS                  EXTNAME(cnind00f)                    *record cnaco
006100971217     d cnclpds       E DS                  EXTNAME(cnclp00f)                    *record cnaco
006200971217     d fnclsds       E DS                  EXTNAME(fncls00f)                    *record cnaco
006300971217     D*------------------
006400971217     D* ARCHITETTURA
006500971217     D*------------------
006600971217     D KPJBA         E DS
006700971217     C*------------------------------------------------------------------------*
006800971217     C* MAIN LINES
006900971217     C*------------------------------------------------------------------------*
007000971217     C*
007100971217     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007200971217DO  1C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007300971217     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
007400971217     C     FINL00        OREQ      '1'                                          *FINE PGM
007500971217     C*
007600971217     C* EMETTE FORMATO VIDEO
007700971217     C                   EXSR      BUTFMR
007800971217     C*
007900971217     C* GESTIONE TASTI FUNZIONALI
008000971217     C                   EXSR      FUNCON
008100971217E   1C                   ENDDO
008200971217     C*
008300971217     C                   SETON                                        LR
008400971217     C*------------------------------------------------------------------------*
008500971217     C* BUTFMR - EMISSIONE FORMATO A VIDEO
008600971217     C*------------------------------------------------------------------------*
008700971217     C     BUTFMR        BEGSR
008800971217     C*
008900971217     C* EMETTE LA TESTATA
009000971217     C                   WRITE     BS70D01
009100971217     C*
009200971217     C* SPEDISCE EVENTUALE MESSAGGIO
009300971217     C                   EXSR      SNDMSG
009400971217     C*
009500971217     C* EMETTE LE FUNZIONI
009600971217     C                   WRITE     BS70D02
009700971217     C*
009800971217     C* EMETTE IL VIDEO
009900971217     C                   EXFMT     BS70D03
010000971217     C*
010100971217     C* REIMPOSTA FLAG DI ERRORE
010200971217     C                   MOVEL     '0'           ERRL00
010300971217     C*
010400971217     C* REIMPOSTA INDICATORI DI ERRORE
010500971217     C                   SETOFF                                       59
010600971217     C                   SETOFF                                       606162
010700971217     C                   SETOFF                                       636465
010800971217     C                   SETOFF                                       666768
010900971217     C                   SETOFF                                       697071
011000971217     C                   SETOFF                                       727374
011100971217     C                   SETOFF                                       757677
011200971217     C                   SETOFF                                       787980
011300971217     C                   SETOFF                                       818283
011400971217     C                   SETOFF                                       848586
011500971217     C                   SETOFF                                       878889
011600971217     C*
011700971217     C                   ENDSR
011800971217     C*------------------------------------------------------------------------*
011900971217     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012000971217     C*------------------------------------------------------------------------*
012100971217     C     SNDMSG        BEGSR
012200971217     C*
012300971217     C* PULISCE IL SFILE MESSAGGI
012400971217     C                   MOVEL     '1'           *IN40
012500971217     C                   WRITE     BS70DMC
012600971217     C                   MOVEL     '0'           *IN40
012700971217     C*
012800971217     C* VISUALIZZA IL MESSAGGIO
012900971217     C                   Z-ADD     *ZEROS        MSGULT            4 0
013000971217     C                   CALL      'XMSG'        XMSGPA
013100971217IF  1C     DSMSER        IFEQ      *ZEROS
013200971217     C                   MOVEL     DSMSKE        MSGKEY
013300971217E   1C                   ENDIF
013400971217     C                   Z-ADD     1             MSGULT
013500971217     C                   WRITE     BS70DMS
013600971217     C                   WRITE     BS70DMC
013700971217     C*
013800971217     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013900971217     C                   MOVEL     *BLANKS       DSMSMS
014000971217     C*
014100971217     C                   ENDSR
014200971217     C*------------------------------------------------------------------------*
014300971217     C* ENTGES - GESTIONE ENTER -> INVIO
014400971217     C*------------------------------------------------------------------------*
014500971217     C     ENTGES        BEGSR
014600971217     C*
014700971217     c                   CLEAR                   cnacods
014800971217     c                   CLEAR                   cnindds
014900971217     c                   CLEAR                   cnclpds
015000971217     c                   CLEAR                   fnclsds
015100971217     c                   CALL      'TIBS69R'
015200971217     c                   PARM                    bs69ds
015300971217     c                   PARM                    cnacods
015400971217     c                   PARM                    cnindds
015500971217     c                   PARM                    cnclpds
015600971217     c                   PARM                    fnclsds
015700971217     C*
015800971217     C                   ENDSR
015900971217     C*------------------------------------------------------------------------*
016000971217     C* F03GES - GESTIONE F03 -> FINE
016100971217     C*------------------------------------------------------------------------*
016200971217     C     F03GES        BEGSR
016300971217     C*
016400971217     C* IMPOSTA "PREMUTO F03"
016500971217     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016600971217     C*
016700971217     C                   ENDSR
016800971217     C*------------------------------------------------------------------------*
016900971217     C* F12GES - GESTIONE F12 -> RITORNO
017000971217     C*------------------------------------------------------------------------*
017100971217     C     F12GES        BEGSR
017200971217     C*
017300971217     C* IMPOSTA "PREMUTO F12"
017400971217     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017500971217     C*
017600971217     C                   ENDSR
017700971217     C*------------------------------------------------------------------------*
017800971217     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017900971217     C*------------------------------------------------------------------------*
018000971217     C     F24GES        BEGSR
018100971217     C*
018200971217     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
018300971217     C*
018400971217     C                   ENDSR
018500971217     C*------------------------------------------------------------------------*
018600971217     C* *INZSR - OPERAZIONI INIZIALI
018700971217     C*------------------------------------------------------------------------*
018800971217     C     *INZSR        BEGSR
018900971217     C*---
019000971217     C* RICEVIMENTO PARAMETRI
019100971217     C*---
019200971217     C     *ENTRY        PLIST
019300971217     C                   PARM                    KPJBA
019400971217     C*
019500971217     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
019600971217     C     XMSGPA        PLIST
019700971217     C                   PARM                    DSMSMS
019800971217     C                   PARM                    DSMSPG
019900971217     C                   PARM                    DSMSKE
020000971217     C                   PARM                    DSMSER
020100971217     C*
020200971217     C* IMPOSTA LA DS -TASTI PREMUTI-
020300971217     C                   CALL      'XKEY'
020400971217     C                   PARM                    DSKEY
020500971217     C*---
020600971217     C* VARIABILI NON RIFERITE AL DATA BASE
020700971217     C*---
020800971217     C     *LIKE         DEFINE    FUDFUN        FUNKEY
020900971217     C*---
021000971217     C* VARIABILI DI CONTROLLO
021100971217     C*---
021200971217     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
021300971217     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
021400971217     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
021500971217     C                   MOVEL     '0'           ERRL00            1            *ERRORE
021600971217     C*
021700971217     C* IMPOSTA LA DS -INVIO MESSAGGI-
021800971217     C                   MOVEL     'TIBS70R'     DSMSPG
021900971217     C                   MOVEL     'TIBS70R'     MSGQUE
022000971217     C                   MOVEL     *BLANKS       MSGKEY
022100971217     C                   MOVEL     *BLANKS       DSMSMS
022200971217     C*
022300971217     C* GESTIONE TASTI FUNZIONALI A VIDEO
022400971217     C                   Z-ADD     *ZEROS        FUNGRU            5 0
022500971217     C                   EXSR      FUNGES
022600971217     C*
022700971217     C* IMPOSTA IL VIDEO
022800971217     c                   CLEAR                   bs69ds
022900971217     c                   CLEAR                   cnacods
023000971217     c                   CLEAR                   cnindds
023100971217     c                   CLEAR                   cnclpds
023200971217     c                   CLEAR                   fnclsds
023300971217     C*
023400971217     C                   ENDSR
023500971217     C*------------------------------------------------------------------------*
023600971217     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
023700971217     C*------------------------------------------------------------------------*
023800971217     C     FUNGES        BEGSR
023900971217     C*
024000971217     C* PULISCO CAMPI DI LAVORO
024100971217     C                   Z-ADD     0             W                 5 0
024200971217     C*
024300971217     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
024400971217     C* E CARICO TABELLA FUNZIONI ATTIVE
024500971217     C     1             DO        24            I
024600971217     C                   MOVEL     FUD(I)        FUDRIG
024700971217     C*
024800971217     C* PULIZIA FUNZIONI PRECEDENTI
024900971217     C                   MOVEL     *BLANKS       FUV(I)
025000971217     C*
025100971217     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
025200971217     C* (SALTA F24)
025300971217     C     FUDATT        IFEQ      'S'
025400971217     C     FUDFUN        ANDNE     '24'
025500971217     C                   ADD       1             W
025600971217     C                   MOVEL     FUDDES        FUV(W)
025700971217     C                   END
025800971217     C                   END                                                    DO WHILE
025900971217     C*
026000971217     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
026100971217     C* ALTRIMENTI DISATTIVO F24
026200971217     C     1             DO        24            I
026300971217     C                   MOVEL     FUD(I)        FUDRIG
026400971217     C*
026500971217     C     FUDFUN        IFEQ      '24'
026600971217     C     W             IFGT      6
026700971217     C                   MOVEL     'S'           FUDATT
026800971217     C                   ELSE
026900971217     C                   MOVEL     *BLANKS       FUDATT
027000971217     C                   END                                                    W GT 8
027100971217     C                   MOVEL     FUDRIG        FUD(I)
027200971217     C                   END
027300971217     C                   END                                                    DO WHILE
027400971217     C*
027500971217     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
027600971217     C     W             DIV       6             I
027700971217     C                   MVR                     Z
027800971217     C*
027900971217     C     Z             IFNE      0
028000971217     C     I             ADD       1             Z
028100971217     C                   ELSE
028200971217     C                   Z-ADD     I             Z                 5 0
028300971217     C                   END
028400971217     C*
028500971217     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
028600971217     C                   ADD       1             FUNGRU
028700971217     C     FUNGRU        IFGT      Z
028800971217     C     FUNGRU        ORLT      1
028900971217     C                   Z-ADD     1             FUNGRU
029000971217     C                   END
029100971217     C*
029200971217     C* PRIMO GRUPPO DI 6 FUNZIONI
029300971217     C     FUNGRU        IFEQ      1
029400971217     C                   MOVEL     FUV(1)        FUN001
029500971217     C                   MOVEL     FUV(2)        FUN002
029600971217     C                   MOVEL     FUV(3)        FUN003
029700971217     C                   MOVEL     FUV(4)        FUN004
029800971217     C                   MOVEL     FUV(5)        FUN005
029900971217     C                   MOVEL     FUV(6)        FUN006
030000971217     C                   END
030100971217     C*
030200971217     C* SECONDO GRUPPO DI 6 FUNZIONI
030300971217     C     FUNGRU        IFEQ      2
030400971217     C                   MOVEL     FUV(7)        FUN001
030500971217     C                   MOVEL     FUV(8)        FUN002
030600971217     C                   MOVEL     FUV(9)        FUN003
030700971217     C                   MOVEL     FUV(10)       FUN004
030800971217     C                   MOVEL     FUV(11)       FUN005
030900971217     C                   MOVEL     FUV(12)       FUN006
031000971217     C                   END
031100971217     C*
031200971217     C* TERZO GRUPPO DI 6 FUNZIONI
031300971217     C     FUNGRU        IFEQ      3
031400971217     C                   MOVEL     FUV(13)       FUN001
031500971217     C                   MOVEL     FUV(14)       FUN002
031600971217     C                   MOVEL     FUV(15)       FUN003
031700971217     C                   MOVEL     FUV(16)       FUN004
031800971217     C                   MOVEL     FUV(17)       FUN005
031900971217     C                   MOVEL     FUV(18)       FUN006
032000971217     C                   END
032100971217     C*
032200971217     C* QUARTO GRUPPO DI 6 FUNZIONI
032300971217     C     FUNGRU        IFEQ      4
032400971217     C                   MOVEL     FUV(19)       FUN001
032500971217     C                   MOVEL     FUV(20)       FUN002
032600971217     C                   MOVEL     FUV(21)       FUN003
032700971217     C                   MOVEL     FUV(22)       FUN004
032800971217     C                   MOVEL     *BLANKS       FUN005
032900971217     C                   MOVEL     *BLANKS       FUN006
033000971217     C                   END
033100971217     C*
033200971217     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
033300971217     C                   MOVEL     *BLANKS       FUN007
033400971217     C                   MOVEL     *BLANKS       FUN008
033500971217     C*
033600971217     C     1             DO        24            I
033700971217     C                   MOVEL     FUD(I)        FUDRIG
033800971217     C     FUDFUN        IFEQ      '24'
033900971217     C     FUDATT        ANDEQ     'S'
034000971217     C                   MOVEL     FUDDES        FUN008
034100971217     C                   END
034200971217     C                   END                                                    DO
034300971217     C*
034400971217     C                   ENDSR
034500971217     C*------------------------------------------------------------------------*
034600971217     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
034700971217     C*------------------------------------------------------------------------*
034800971217     C     FUNCON        BEGSR
034900971217     C*
035000971217     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
035100971217     C                   EXSR      FUNVAR
035200971217     C*
035300971217     C* PULISCO CAMPI DI LAVORO
035400971217     C                   Z-ADD     0             I                 5 0
035500971217     C                   Z-ADD     0             W                 5 0
035600971217     C*
035700971217     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
035800971217     C     FUNKEY        IFNE      *BLANKS
035900971217     C     1             DO        24            I
036000971217     C                   MOVEL     FUD(I)        FUDRIG
036100971217     C*
036200971217     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
036300971217     C     FUNKEY        IFEQ      FUDFUN
036400971217     C     FUDATT        ANDEQ     'S'
036500971217     C                   Z-ADD     I             W
036600971217     C                   END
036700971217     C                   END                                                    DO WHILE
036800971217     C*
036900971217     C* FUNZIONE NON ATTIVA (ERRORE)
037000971217     C     W             IFEQ      0
037100971217     C*
037200971217     C* EMISSIONE MESSAGGIO DI ERRORE
037300971217     C                   MOVEL     MSG(1)        DSMSMS
037400971217     C*
037500971217     C                   END                                                    W=0
037600971217     C                   END                                                    FUNKEY NE *BLANKS
037700971217     C*
037800971217     C* FUNZIONE ATTIVA (PROCEDI ...)
037900971217     C     W             IFNE      0
038000971217     C     FUNKEY        OREQ      *BLANKS
038100971217     C     FUNKEY        IFEQ      *BLANKS
038200971217     C                   EXSR      ENTGES
038300971217     C                   END
038400971217     C     FUNKEY        IFEQ      '03'
038500971217     C                   EXSR      F03GES
038600971217     C                   END
038700971217     C     FUNKEY        IFEQ      '12'
038800971217     C                   EXSR      F12GES
038900971217     C                   END
039000971217     C                   END
039100971217     C*
039200971217     C                   ENDSR
039300971217     C*------------------------------------------------------------------------*
039400971217     C* AZZVID - AZZERA I CAMPI DEL VIDEO
039500971217     C*------------------------------------------------------------------------*
039600971217     C     AZZVID        BEGSR
039700971217     C*
039800971217     c                   MOVEL     knsif         i69sif
039900971217     C*
040000971217     C                   ENDSR
040100971217     C*------------------------------------------------------------------------*
040200971217     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
040300971217     C*------------------------------------------------------------------------*
040400971217     C     FUNVAR        BEGSR
040500971217     C*
040600971217     C* INIZIALIZZA VARIABILE -FUNKEY-
040700971217     C*
040800971217     C* FUNKEY=*BLANKS SIGNIFICA ENTER
040900971217     C     KEY           IFEQ      ENTER
041000971217     C                   MOVEL     *BLANKS       FUNKEY
041100971217     C                   END
041200971217     C     KEY           IFEQ      F1
041300971217     C                   MOVEL     '01'          FUNKEY
041400971217     C                   END
041500971217     C     KEY           IFEQ      F2
041600971217     C                   MOVEL     '02'          FUNKEY
041700971217     C                   END
041800971217     C     KEY           IFEQ      F3
041900971217     C                   MOVEL     '03'          FUNKEY
042000971217     C                   END
042100971217     C     KEY           IFEQ      F4
042200971217     C                   MOVEL     '04'          FUNKEY
042300971217     C                   END
042400971217     C     KEY           IFEQ      F5
042500971217     C                   MOVEL     '05'          FUNKEY
042600971217     C                   END
042700971217     C     KEY           IFEQ      F6
042800971217     C                   MOVEL     '06'          FUNKEY
042900971217     C                   END
043000971217     C     KEY           IFEQ      F7
043100971217     C                   MOVEL     '07'          FUNKEY
043200971217     C                   END
043300971217     C     KEY           IFEQ      F8
043400971217     C                   MOVEL     '08'          FUNKEY
043500971217     C                   END
043600971217     C     KEY           IFEQ      F9
043700971217     C                   MOVEL     '09'          FUNKEY
043800971217     C                   END
043900971217     C     KEY           IFEQ      F10
044000971217     C                   MOVEL     '10'          FUNKEY
044100971217     C                   END
044200971217     C     KEY           IFEQ      F11
044300971217     C                   MOVEL     '11'          FUNKEY
044400971217     C                   END
044500971217     C     KEY           IFEQ      F12
044600971217     C                   MOVEL     '12'          FUNKEY
044700971217     C                   END
044800971217     C     KEY           IFEQ      F13
044900971217     C                   MOVEL     '13'          FUNKEY
045000971217     C                   END
045100971217     C     KEY           IFEQ      F14
045200971217     C                   MOVEL     '14'          FUNKEY
045300971217     C                   END
045400971217     C     KEY           IFEQ      F15
045500971217     C                   MOVEL     '15'          FUNKEY
045600971217     C                   END
045700971217     C     KEY           IFEQ      F16
045800971217     C                   MOVEL     '16'          FUNKEY
045900971217     C                   END
046000971217     C     KEY           IFEQ      F17
046100971217     C                   MOVEL     '17'          FUNKEY
046200971217     C                   END
046300971217     C     KEY           IFEQ      F18
046400971217     C                   MOVEL     '18'          FUNKEY
046500971217     C                   END
046600971217     C     KEY           IFEQ      F19
046700971217     C                   MOVEL     '19'          FUNKEY
046800971217     C                   END
046900971217     C     KEY           IFEQ      F20
047000971217     C                   MOVEL     '20'          FUNKEY
047100971217     C                   END
047200971217     C     KEY           IFEQ      F21
047300971217     C                   MOVEL     '21'          FUNKEY
047400971217     C                   END
047500971217     C     KEY           IFEQ      F22
047600971217     C                   MOVEL     '22'          FUNKEY
047700971217     C                   END
047800971217     C     KEY           IFEQ      F23
047900971217     C                   MOVEL     '23'          FUNKEY
048000971217     C                   END
048100971217     C     KEY           IFEQ      F24
048200971217     C                   MOVEL     '24'          FUNKEY
048300971217     C                   END
048400971217     C     KEY           IFEQ      ROLLUP
048500971217     C                   MOVEL     'RU'          FUNKEY
048600971217     C                   END
048700971217     C     KEY           IFEQ      HELP
048800971217     C                   MOVEL     'HP'          FUNKEY
048900971217     C                   END
049000971217     C                   ENDSR
049100971217     C*------------------------------------------------------------------------*
049200971217     C* NOTE COSTRUZIONE SCHIERE
049300971217     C*------------------------------------------------------------------------*
049400971217     C*
049500971217     C* -MSG-MESSAGGI
049600971217     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
049700971217     C* (NON TOCCARE MSG,1 E MSG,2)
049800971217     C*
049900971217     C* -CMD-COMANDI
050000971217     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
050100971217     C*
050200971217     C* -OPD-OPZIONI
050300971217     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
050400971217     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
050500971217     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
050600971217     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
050700971217     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
050800971217     C*  . POSSONO ESSERE IN ORDINE SPARSO
050900971217     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
051000971217     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
051100971217     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
051200971217     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
051300971217     C*
051400971217     C* -FUD-FUNZIONI
051500971217     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
051600971217     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
051700971217     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
051800971217     C*  . POSSONO ESSERE IN ORDINE SPARSO
051900971217     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
052000971217     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
052100971217     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
052200971217     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
052300971217     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
052400971217     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
052500971217     o*--------------------------------------------------------------------------------------------*
052600971217** msg - messaggi
052700971217TIBS69R- Errori in apertura CNACO00F                                           1
052800971217TIBS69R- Errori in apertura CNIND00F                                           2
052900971217TIBS69R- Errori in apertura CNCLP00F                                           3
053000971217TIBS69R- Errori in apertura FNCLS00F                                           4
053100971217TIBS69R- Cliente non trovato                                                   5
053200971217TIBS69R-                                                                       6
053300971217TIBS69R-                                                                       7
053400971217TIBS69R-                                                                       8
053500971217TIBS69R-                                                                       9
053600971217TIBS69R-                                                                      10
053700971217** FUD - FUNZIONI
053800971217                   -        (S/N)=N                        funz.01¬
053900971217                   -        (S/N)=N                        funz.02¬
054000971217F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
054100971217                   -        (S/N)=N                        funz.04¬
054200971217                   -        (S/N)=N                        funz.05¬
054300971217                   -        (S/N)=N                        funz.06¬
054400971217                   -        (S/N)=N                        funz.07¬
054500971217                   -        (S/N)=N                        funz.08¬
054600971217                   -        (S/N)=N                        funz.09¬
054700971217                   -        (S/N)=N                        funz.10¬
054800971217                   -        (S/N)=N                        funz.11¬
054900971217F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
055000971217                   -        (S/N)=N                        funz.13¬
055100971217                   -        (S/N)=N                        funz.14¬
055200971217                   -        (S/N)=N                        funz.15¬
055300971217                   -        (S/N)=N                        funz.16¬
055400971217                   -        (S/N)=N                        funz.17¬
055500971217                   -        (S/N)=N                        funz.18¬
055600971217                   -        (S/N)=N                        funz.19¬
055700971217                   -        (S/N)=N                        funz.20¬
055800971217                   -        (S/N)=N                        funz.21¬
055900971217                   -        (S/N)=N                        funz.22¬
056000971217                   -        (S/N)=N                        funz.23¬
056100971217F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

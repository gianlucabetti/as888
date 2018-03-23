000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600050816     FTRTCM10D  CF   E             WORKSTN INFDS(DEVDS1)
000700050816     F                                     SFILE(TCM0DMS:MSGULT)
000800050816     FPRTF198   O    F  198        PRINTER OFLIND(*INOF) USROPN
000900970526     D*------------------------------------------------------------------------*
001000970526     D* SCHIERE
001100970526     D*------------------------------------------------------------------------*
001200970526     D*------------
001300970526     D* DECODIFICHE
001400970526     D*------------
001500020415     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* MESSAGGI
001800970526     D*------------
001900970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800970526     D*------------------------------------------------------------------------*
002900970526     D* INPUT
003000970526     D*------------------------------------------------------------------------*
003100970526     D*------------
003200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003300970526     D*------------
003400970526     D FUDDS           DS
003500970526     D  FUDDE1                 1      1
003600970526     D  FUDFUN                 2      3
003700970526     D  FUDDE2                 4     19
003800970526     D  FUDFI0                20     34
003900970526     D  FUDATT                35     35
004000970526     D  FUDFI1                36     80
004100970526     D  FUDDES                 1     19
004200970526     D  FUDRIG                 1     80
004300970526     D*------------
004400970526     D* DS GESTIONE TASTI FUNZIONALI
004500970526     D*------------
004600970526     D DSKEY         E DS
004700970526     D DEVDS1          DS
004800970526     D  KEY                  369    369
004900970526     D*------------
005000970526     D* DS "XMSG" - GESTIONE MESSAGGI
005100970526     D*------------
005200970526     D DSMSG         E DS
005300970526     D*------------
005400020415     D*------------
005500970526     D* ARCHITETTURA
005600970526     D*------------
005700970526     D KPJBA         E DS
005800000407     D*------------------
005900020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
006000020412     D*------------------
006100050816     D TRTCM1DS      E DS
006200050816     D*------------------
006300050816     D* Parametri x modifiche attributi spool
006400050816     D*------------------
006500050816     D PARAM           S            262    DIM(20)
006600050816     D*------------------
006700050816     D* DS scomposizione parametri x modifiche attributi spool
006800050816     D*------------------
006900050816     D SPOOLATR        DS
007000050816     D  POSDA                  1      4  0
007100050816     D  LUNG                   5      7  0
007200050816     D  VALORE                 8    262
007300970526     C*------------------------------------------------------------------------*
007400970526     C* MAIN LINES
007500970526     C*------------------------------------------------------------------------*
007600970526     C*
007700970526     C* NON CI SONO ERRORI IN ENTRATA
007800970526IF  1C     ERRL00        IFEQ      '0'
007900970526     C*
008000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008400970526     C*
008500970526     C* EMETTE FORMATO VIDEO
008600970526     C                   EXSR      BUTFMR
008700970526     C*
008800970526     C* GESTIONE TASTI FUNZIONALI
008900970526     C                   EXSR      FUNCON
009000970526E   2C                   ENDDO
009100970526     C*
009200970526E   1C                   ENDIF
009300970526     C* FINE PGM
009400970526     C                   SETON                                        LR
009500970526     C*------------------------------------------------------------------------*
009600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009700970526     C*------------------------------------------------------------------------*
009800970526     C     BUTFMR        BEGSR
009900970526     C*
010000970526     C* EMETTE LA TESTATA
010100050816     C                   WRITE     TCM0D01
010200970526     C*
010300970526     C* SPEDISCE EVENTUALE MESSAGGIO
010400970526     C                   EXSR      SNDMSG
010500970526     C*
010600970526     C* EMETTE LE FUNZIONI
010700050816     C                   WRITE     TCM0D02
010800970526     C*
010900970526     C* EMETTE IL VIDEO
011000050816     C                   EXFMT     TCM0D03
011100970526     C*
011200970526     C* REIMPOSTA FLAG DI ERRORE
011300970526     C                   MOVEL     '0'           ERRL00
011400970526     C*
011500970526     C                   ENDSR
011600970526     C*------------------------------------------------------------------------*
011700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011800970526     C*------------------------------------------------------------------------*
011900970526     C     SNDMSG        BEGSR
012000970526     C*
012100970526     C* PULISCE IL SFILE MESSAGGI
012200970526     C                   MOVEL     '1'           *IN40
012300050816     C                   WRITE     TCM0DMC
012400970526     C                   MOVEL     '0'           *IN40
012500970526     C*
012600970526     C* VISUALIZZA IL MESSAGGIO
012700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012800970526     C                   CALL      'XMSG'        XMSGPA
012900970526IF  1C     DSMSER        IFEQ      *ZEROS
013000970526     C                   MOVEL     DSMSKE        MSGKEY
013100970526E   1C                   ENDIF
013200970526     C                   Z-ADD     1             MSGULT
013300050816     C                   WRITE     TCM0DMS
013400050816     C                   WRITE     TCM0DMC
013500970526     C*
013600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013700970526     C                   MOVEL     *BLANKS       DSMSMS
013800970526     C*
013900970526     C                   ENDSR
014000970526     C*------------------------------------------------------------------------*
014100970526     C* ENTGES - GESTIONE ENTER -> INVIO
014200970526     C*------------------------------------------------------------------------*
014300970526     C     ENTGES        BEGSR
014400970526     C*
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F03GES - GESTIONE F03 -> FINE
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F03GES        BEGSR
015100970526     C*
015200970526     C* IMPOSTA "PREMUTO F03"
015300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700050816     C* F06GES - GESTIONE F06 -> CONFERMA OPZIONE 1
015800970526     C*------------------------------------------------------------------------*
015900050816     C     F06GES        BEGSR
016000970526     C*
016100050816     C* ESEGUO OVERRIDE AL PRTF
016200050816     C                   CALL(e)   'TRTCM10C'
016300050816     C                   PARM                    TRTCM1DS
016400050816     C                   PARM      'O'           OPE               1
016500050816     C                   IF        not %error
016600050816     C*
016700050816     C*  APRO IL FILE D SPOOL
016800050816     C                   IF        not %open(PRTF198)
016900050816     C                   OPEN      PRTF198
017000050816     C                   ENDIF
017100050816     C*
017200050816     C* ESEGUO STAMPA
017300050816     C                   EXCEPT    WRIPRTF
017400050816     C*
017500050816     C*  CHIUDO IL FILE D SPOOL
017600050816     C                   IF        %open(PRTF198)
017700050816     C                   CLOSE     PRTF198
017800050816     C                   ENDIF
017900050816     C*
018000050816     C* RIMUOVO OVERRIDE AL PRTF
018100050816     C                   CALL(e)   'TRTCM10C'
018200050816     C                   PARM                    TRTCM1DS
018300050816     C                   PARM      'D'           OPE
018400050816     C                   ENDIF
018500020412     C*
018600020412     C                   ENDSR
018700050816     C*------------------------------------------------------------------------*
018800050816     C* F07GES - GESTIONE F07 -> CONFERMA OPZIONE 2
018900050816     C*------------------------------------------------------------------------*
019000050816     C     F07GES        BEGSR
019100050816     C*
019200050816     C* Modifico gli attributi dello spool scelto x effettuare la copia x l'elaborazione
019300050816     C                   CLEAR                   PARAM
019400050816     C*
019500050816     C* Modifico a *NO il salvataggio del file d spool dopo la scrittura
019600050816     C                   CLEAR                   SPOOLATR
019700050816     C                   EVAL      POSDA = 138
019800050816     C                   EVAL      LUNG  = 10
019900050816     C                   EVAL      VALORE = '*NO'
020000050816     C                   EVAL      PARAM(1) = SPOOLATR
020100050816     C*
020200050816     C* Modifico l'utente proprietario dello spool "destinazione"
020300050816     C                   CLEAR                   SPOOLATR
020400050816     C                   EVAL      POSDA = 58
020500050816     C                   EVAL      LUNG  = 10
020600050816     C                   EVAL      VALORE = 'EDPMAIL'
020700050816     C                   EVAL      PARAM(2) = SPOOLATR
020800050816     C*
020900050816     C* Modifico il parametro USRDFNDTA (dati utente estesi) con quanto indicato a video dall'utente
021000050816     C                   CLEAR                   SPOOLATR
021100050816     C                   EVAL      POSDA = 3460
021200050816     C                   EVAL      LUNG  = 255
021300050816     C                   EVAL      VALORE = TRTCM1DS
021400050816     C                   EVAL      PARAM(5) = SPOOLATR
021500050816     C*
021600050816     C* Modifico lo stato dello spool "destinazione" in modo che sia sempre a *RDY
021700050816     C                   CLEAR                   SPOOLATR
021800050816     C                   EVAL      POSDA = 108
021900050816     C                   EVAL      LUNG  = 10
022000050816     C                   EVAL      VALORE = '*READY'
022100050816     C                   EVAL      PARAM(6) = SPOOLATR
022200050816     C*
022300050816     C* Modifico a *NO il precongelamento prima della scrittura
022400050816     C                   CLEAR                   SPOOLATR
022500050816     C                   EVAL      POSDA = 128
022600050816     C                   EVAL      LUNG  = 10
022700050816     C                   EVAL      VALORE = '*NO'
022800050816     C                   EVAL      PARAM(7) = SPOOLATR
022900050816     C*
023000050816     C* Effettuo la copia dello spool con gli attributi così modificati
023100050816     C                   CALL      'XDUPSPLFR'
023200050816     C                   PARM                    VIDFILE
023300050816     C                   PARM                    VIDJOB
023400050816     C                   PARM                    VIDUTE
023500050816     C                   PARM                    VIDNUMJOB
023600050816     C                   PARM                    VIDNUMFIL
023700050816     C                   PARM                    PARAM
023800050816     C*
023900050816     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100970526     C* F12GES - GESTIONE F12 -> RITORNO
024200970526     C*------------------------------------------------------------------------*
024300970526     C     F12GES        BEGSR
024400970526     C*
024500970526     C* IMPOSTA "PREMUTO F12"
024600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025100970526     C*------------------------------------------------------------------------*
025200970526     C     F24GES        BEGSR
025300970526     C*
025400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025500970526     C*
025600970526     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025900970526     C*------------------------------------------------------------------------*
026000970526     C     IMPFUN        BEGSR
026100970526     C*
026200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
026300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
026400970526     C                   EXSR      FUNGES
026500970526     C*
026600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
026700970526     C                   MOVEL     DEC(1)        VIDFUN
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100970526     C* *INZSR - OPERAZIONI INIZIALI
027200970526     C*------------------------------------------------------------------------*
027300970526     C     *INZSR        BEGSR
027400970526     C*---
027500970526     C* RICEVIMENTO PARAMETRI
027600970526     C*---
027700970526     C     *ENTRY        PLIST
027800970612     C                   PARM                    KPJBA
027900970526     C*
028000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
028100970526     C     XMSGPA        PLIST
028200970526     C                   PARM                    DSMSMS
028300970526     C                   PARM                    DSMSPG
028400970526     C                   PARM                    DSMSKE
028500970526     C                   PARM                    DSMSER
028600970526     C*
028700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
028800970526     C                   CALL      'XKEY'
028900970526     C                   PARM                    DSKEY
029000970526     C*---
029100970526     C* VARIABILI NON RIFERITE AL DATA BASE
029200970526     C*---
029300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
029400970526     C*---
029500970526     C* VARIABILI DI CONTROLLO
029600970526     C*---
029700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
029800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
029900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
030000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
030100970526     C*
030200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
030300050816     C                   MOVEL     'TRTCM10R'    DSMSPG
030400050816     C                   MOVEL     'TRTCM10R'    MSGQUE
030500970526     C                   MOVEL     *BLANKS       MSGKEY
030600970526     C                   MOVEL     *BLANKS       DSMSMS
030700970526     C*
030800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
030900970526     C                   EXSR      IMPFUN
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
031400970526     C*------------------------------------------------------------------------*
031500970526     C     FUNGES        BEGSR
031600970526     C*
031700970526     C* PULISCO CAMPI DI LAVORO
031800970526     C                   Z-ADD     0             W                 5 0
031900970526     C*
032000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
032100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
032200970526     C     1             DO        24            I
032300970526     C                   MOVEL     FUD(I)        FUDRIG
032400970526     C*
032500970526     C* PULIZIA FUNZIONI PRECEDENTI
032600970526     C                   MOVEL     *BLANKS       FUV(I)
032700970526     C*
032800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
032900970526     C* (SALTA F24)
033000970526     C     FUDATT        IFEQ      'S'
033100970526     C     FUDFUN        ANDNE     '24'
033200970526     C                   ADD       1             W
033300970526     C                   MOVEL     FUDDES        FUV(W)
033400970526     C                   END
033500970526     C                   END                                                    DO WHILE
033600970526     C*
033700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
033800970526     C* ALTRIMENTI DISATTIVO F24
033900970526     C     1             DO        24            I
034000970526     C                   MOVEL     FUD(I)        FUDRIG
034100970526     C*
034200970526     C     FUDFUN        IFEQ      '24'
034300970526     C     W             IFGT      6
034400970526     C                   MOVEL     'S'           FUDATT
034500970526     C                   ELSE
034600970526     C                   MOVEL     *BLANKS       FUDATT
034700970526     C                   END                                                    W GT 8
034800970526     C                   MOVEL     FUDRIG        FUD(I)
034900970526     C                   END
035000970526     C                   END                                                    DO WHILE
035100970526     C*
035200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
035300970526     C     W             DIV       6             I
035400970526     C                   MVR                     Z
035500970526     C*
035600970526     C     Z             IFNE      0
035700970526     C     I             ADD       1             Z
035800970526     C                   ELSE
035900970526     C                   Z-ADD     I             Z                 5 0
036000970526     C                   END
036100970526     C*
036200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
036300970526     C                   ADD       1             FUNGRU
036400970526     C     FUNGRU        IFGT      Z
036500970526     C     FUNGRU        ORLT      1
036600970526     C                   Z-ADD     1             FUNGRU
036700970526     C                   END
036800970526     C*
036900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
037000970526     C     FUNGRU        IFEQ      1
037100970526     C                   MOVEL     FUV(1)        FUN001
037200970526     C                   MOVEL     FUV(2)        FUN002
037300970526     C                   MOVEL     FUV(3)        FUN003
037400970526     C                   MOVEL     FUV(4)        FUN004
037500970526     C                   MOVEL     FUV(5)        FUN005
037600970526     C                   MOVEL     FUV(6)        FUN006
037700970526     C                   END
037800970526     C*
037900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
038000970526     C     FUNGRU        IFEQ      2
038100970526     C                   MOVEL     FUV(7)        FUN001
038200970526     C                   MOVEL     FUV(8)        FUN002
038300970526     C                   MOVEL     FUV(9)        FUN003
038400970526     C                   MOVEL     FUV(10)       FUN004
038500970526     C                   MOVEL     FUV(11)       FUN005
038600970526     C                   MOVEL     FUV(12)       FUN006
038700970526     C                   END
038800970526     C*
038900970526     C* TERZO GRUPPO DI 6 FUNZIONI
039000970526     C     FUNGRU        IFEQ      3
039100970526     C                   MOVEL     FUV(13)       FUN001
039200970526     C                   MOVEL     FUV(14)       FUN002
039300970526     C                   MOVEL     FUV(15)       FUN003
039400970526     C                   MOVEL     FUV(16)       FUN004
039500970526     C                   MOVEL     FUV(17)       FUN005
039600970526     C                   MOVEL     FUV(18)       FUN006
039700970526     C                   END
039800970526     C*
039900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
040000970526     C     FUNGRU        IFEQ      4
040100970526     C                   MOVEL     FUV(19)       FUN001
040200970526     C                   MOVEL     FUV(20)       FUN002
040300970526     C                   MOVEL     FUV(21)       FUN003
040400970526     C                   MOVEL     FUV(22)       FUN004
040500970526     C                   MOVEL     *BLANKS       FUN005
040600970526     C                   MOVEL     *BLANKS       FUN006
040700970526     C                   END
040800970526     C*
040900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
041000970526     C                   MOVEL     *BLANKS       FUN007
041100970526     C                   MOVEL     *BLANKS       FUN008
041200970526     C*
041300970526     C     1             DO        24            I
041400970526     C                   MOVEL     FUD(I)        FUDRIG
041500970526     C     FUDFUN        IFEQ      '24'
041600970526     C     FUDATT        ANDEQ     'S'
041700970526     C                   MOVEL     FUDDES        FUN008
041800970526     C                   END
041900970526     C                   END                                                    DO
042000970526     C*
042100970526     C                   ENDSR
042200970526     C*------------------------------------------------------------------------*
042300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
042400970526     C*------------------------------------------------------------------------*
042500970526     C     FUNCON        BEGSR
042600970526     C*
042700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
042800970526     C                   EXSR      FUNVAR
042900970526     C*
043000970526     C* PULISCO CAMPI DI LAVORO
043100970526     C                   Z-ADD     0             I                 5 0
043200970526     C                   Z-ADD     0             W                 5 0
043300970526     C*
043400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
043500970526     C     FUNKEY        IFNE      *BLANKS
043600970526     C     1             DO        24            I
043700970526     C                   MOVEL     FUD(I)        FUDRIG
043800970526     C*
043900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
044000970526     C     FUNKEY        IFEQ      FUDFUN
044100970526     C     FUDATT        ANDEQ     'S'
044200970526     C                   Z-ADD     I             W
044300970526     C                   END
044400970526     C                   END                                                    DO WHILE
044500970526     C*
044600970526     C* FUNZIONE NON ATTIVA (ERRORE)
044700970526     C     W             IFEQ      0
044800970526     C*
044900970526     C* EMISSIONE MESSAGGIO DI ERRORE
045000970526     C                   MOVEL     MSG(1)        DSMSMS
045100970526     C*
045200970526     C                   END                                                    W=0
045300970526     C                   END                                                    FUNKEY NE *BLANKS
045400970526     C*
045500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
045600970526     C     W             IFNE      0
045700970526     C     FUNKEY        OREQ      *BLANKS
045800970526     C     FUNKEY        IFEQ      *BLANKS
045900970526     C                   EXSR      ENTGES
046000970526     C                   END
046100970526     C     FUNKEY        IFEQ      '03'
046200970526     C                   EXSR      F03GES
046300970526     C                   END
046400970526     C     FUNKEY        IFEQ      '06'
046500970526     C                   EXSR      F06GES
046600970526     C                   END
046700050816     C     FUNKEY        IFEQ      '07'
046800050816     C                   EXSR      F07GES
046900050816     C                   END
047000970526     C     FUNKEY        IFEQ      '12'
047100970526     C                   EXSR      F12GES
047200970526     C                   END
047300970526     C     FUNKEY        IFEQ      '24'
047400970526     C                   EXSR      F24GES
047500970526     C                   END
047600970526     C                   END
047700970526     C*
047800970526     C                   ENDSR
047900970526     C*------------------------------------------------------------------------*
048000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
048100970526     C*------------------------------------------------------------------------*
048200970526     C     FUNVAR        BEGSR
048300970526     C*
048400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
048500970526     C*
048600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
048700970526     C     KEY           IFEQ      ENTER
048800970526     C                   MOVEL     *BLANKS       FUNKEY
048900970526     C                   END
049000970526     C     KEY           IFEQ      F1
049100970526     C                   MOVEL     '01'          FUNKEY
049200970526     C                   END
049300970526     C     KEY           IFEQ      F2
049400970526     C                   MOVEL     '02'          FUNKEY
049500970526     C                   END
049600970526     C     KEY           IFEQ      F3
049700970526     C                   MOVEL     '03'          FUNKEY
049800970526     C                   END
049900970526     C     KEY           IFEQ      F4
050000970526     C                   MOVEL     '04'          FUNKEY
050100970526     C                   END
050200970526     C     KEY           IFEQ      F5
050300970526     C                   MOVEL     '05'          FUNKEY
050400970526     C                   END
050500970526     C     KEY           IFEQ      F6
050600970526     C                   MOVEL     '06'          FUNKEY
050700970526     C                   END
050800970526     C     KEY           IFEQ      F7
050900970526     C                   MOVEL     '07'          FUNKEY
051000970526     C                   END
051100970526     C     KEY           IFEQ      F8
051200970526     C                   MOVEL     '08'          FUNKEY
051300970526     C                   END
051400970526     C     KEY           IFEQ      F9
051500970526     C                   MOVEL     '09'          FUNKEY
051600970526     C                   END
051700970526     C     KEY           IFEQ      F10
051800970526     C                   MOVEL     '10'          FUNKEY
051900970526     C                   END
052000970526     C     KEY           IFEQ      F11
052100970526     C                   MOVEL     '11'          FUNKEY
052200970526     C                   END
052300970526     C     KEY           IFEQ      F12
052400970526     C                   MOVEL     '12'          FUNKEY
052500970526     C                   END
052600970526     C     KEY           IFEQ      F13
052700970526     C                   MOVEL     '13'          FUNKEY
052800970526     C                   END
052900970526     C     KEY           IFEQ      F14
053000970526     C                   MOVEL     '14'          FUNKEY
053100970526     C                   END
053200970526     C     KEY           IFEQ      F15
053300970526     C                   MOVEL     '15'          FUNKEY
053400970526     C                   END
053500970526     C     KEY           IFEQ      F16
053600970526     C                   MOVEL     '16'          FUNKEY
053700970526     C                   END
053800970526     C     KEY           IFEQ      F17
053900970526     C                   MOVEL     '17'          FUNKEY
054000970526     C                   END
054100970526     C     KEY           IFEQ      F18
054200970526     C                   MOVEL     '18'          FUNKEY
054300970526     C                   END
054400970526     C     KEY           IFEQ      F19
054500970526     C                   MOVEL     '19'          FUNKEY
054600970526     C                   END
054700970526     C     KEY           IFEQ      F20
054800970526     C                   MOVEL     '20'          FUNKEY
054900970526     C                   END
055000970526     C     KEY           IFEQ      F21
055100970526     C                   MOVEL     '21'          FUNKEY
055200970526     C                   END
055300970526     C     KEY           IFEQ      F22
055400970526     C                   MOVEL     '22'          FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F23
055700970526     C                   MOVEL     '23'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F24
056000970526     C                   MOVEL     '24'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      ROLLUP
056300970526     C                   MOVEL     'RU'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      HELP
056600970526     C                   MOVEL     'HP'          FUNKEY
056700970526     C                   END
056800970526     C                   ENDSR
056900970526     C*------------------------------------------------------------------------*
057000970526     C* NOTE COSTRUZIONE SCHIERE
057100970526     C*------------------------------------------------------------------------*
057200970526     C*
057300970526     C* -MSG-MESSAGGI
057400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
057500970526     C* (NON TOCCARE MSG,1 E MSG,2)
057600970526     C*
057700970526     C* -CMD-COMANDI
057800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
057900970526     C*
058000970526     C* -OPD-OPZIONI
058100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
058200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
058300970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
058400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
058500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
058600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
058700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
058800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
058900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
059000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
059100970526     C*
059200970526     C* -FUD-FUNZIONI
059300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
059400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
059500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
059600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
059700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
059800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
059900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
060000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
060100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
060200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
060300050816     O*--------------------------------------------------------------------------------------------*
060400050816     OPRTF198   E            WRIPRTF     1
060500050816     O                       VIDOUTR1
060600050816     O          E            WRIPRTF     1
060700050816     O                       VIDOUTR2
060800050816     O          E            WRIPRTF     1
060900050816     O                       VIDOUTR3
061000970526     O*------------------------------------------------------------------------*
061100970526** DEC - DECODIFICHE
061200020415     TEST                                                         1
061300020415                                                                  2
061400020415                                                                  3
061500020415                                                                  4
061600020415                                                                  5
061700020415                                                                  6
061800970528** MSG - MESSAGGI
061900050816                                                                               1
062000050816                                                                               2
062100050816                                                                               3
062200050816                                                                               4
062300050816                                                                               5
062400050816                                                                               6
062500050816                                                                               7
062600050816                                                                               8
062700050816                                                                               9
062800050816                                                                               10
062900050816                                                                               11
063000050816                                                                               12
063100050816                                                                               13
063200050816                                                                               14
063300050816                                                                               15
063400050816                                                                               16
063500050816                                                                               17
063600020415                                                                               18
063700020415                                                                               19
063800020415                                                                               20
063900970526** FUD - FUNZIONI
064000970526                   -        (S/N)=N                        funz.01¬
064100970526                   -        (S/N)=N                        funz.02¬
064200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
064300970526                   -        (S/N)=N                        funz.04¬
064400970526                   -        (S/N)=N                        funz.05¬
064500050816F06=OK Opz. 1      -        (S/N)=S                        funz.06¬
064600050816F07=OK Opz. 2      -        (S/N)=S                        funz.07¬
064700970527                   -        (S/N)=N                        funz.08¬
064800970527                   -        (S/N)=N                        funz.09¬
064900970527                   -        (S/N)=N                        funz.10¬
065000970526                   -        (S/N)=N                        funz.11¬
065100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
065200970526                   -        (S/N)=N                        funz.13¬
065300970526                   -        (S/N)=N                        funz.14¬
065400970526                   -        (S/N)=N                        funz.15¬
065500970526                   -        (S/N)=N                        funz.16¬
065600970526                   -        (S/N)=N                        funz.17¬
065700970526                   -        (S/N)=N                        funz.18¬
065800970526                   -        (S/N)=N                        funz.19¬
065900970526                   -        (S/N)=N                        funz.20¬
066000970526                   -        (S/N)=N                        funz.21¬
066100970526                   -        (S/N)=N                        funz.22¬
066200970526                   -        (S/N)=N                        funz.23¬
066300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

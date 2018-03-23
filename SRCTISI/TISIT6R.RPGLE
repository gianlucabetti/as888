000100970526     H*------------------------------------------------------------------------*
000200051227     H* TEST - DESTINATARI PARTICOLARI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800051227     FTNTBE01L  IF   E           K DISK
000900020415     FTISIT6D   CF   E             WORKSTN INFDS(DEVDS1)
001000020415     F                                     SFILE(SIT6DMS:MSGULT)
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------
001500970526     D* DECODIFICHE
001600970526     D*------------
001700020415     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005500970526     D*------------
005600051227     D* TABELLE
005700051227     D*------------
005800051227     D DTDP          E DS
005900020415     D*------------
006000970526     D* ARCHITETTURA
006100970526     D*------------
006200970526     D KPJBA         E DS
006300000407     D*------------------
006400020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
006500020412     D*------------------
006600020412     D TISIT0DS      E DS
006700070904     D TISIT0X1DS    E DS
006800061116     D*-------------------
006900061116     D* VARIABILI D WRK
007000061116     D*-------------------
007100061116     D wTimeStr        s               z   inz
007200061116     D wTimeEnd        s               z   inz
007300070904     D wTISIT0X1DS     s             78    inz
007400970526     C*------------------------------------------------------------------------*
007500970526     C* MAIN LINES
007600970526     C*------------------------------------------------------------------------*
007700970526     C*
007800970526     C* NON CI SONO ERRORI IN ENTRATA
007900970526IF  1C     ERRL00        IFEQ      '0'
008000970526     C*
008100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008500970526     C*
008600970526     C* EMETTE FORMATO VIDEO
008700970526     C                   EXSR      BUTFMR
008800970526     C*
008900970526     C* GESTIONE TASTI FUNZIONALI
009000970526     C                   EXSR      FUNCON
009100970526E   2C                   ENDDO
009200970526     C*
009300970526E   1C                   ENDIF
009400970526     C* FINE PGM
009500970526     C                   SETON                                        LR
009600970526     C*------------------------------------------------------------------------*
009700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009800970526     C*------------------------------------------------------------------------*
009900970526     C     BUTFMR        BEGSR
010000970526     C*
010100970526     C* EMETTE LA TESTATA
010200020415     C                   WRITE     SIT6D01
010300970526     C*
010400970526     C* SPEDISCE EVENTUALE MESSAGGIO
010500970526     C                   EXSR      SNDMSG
010600970526     C*
010700970526     C* EMETTE LE FUNZIONI
010800020415     C                   WRITE     SIT6D02
010900970526     C*
011000970526     C* EMETTE IL VIDEO
011100020415     C                   EXFMT     SIT6D03
011200970526     C*
011300970526     C* REIMPOSTA FLAG DI ERRORE
011400970526     C                   MOVEL     '0'           ERRL00
011500970526     C*
011600970526     C                   ENDSR
011700970526     C*------------------------------------------------------------------------*
011800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011900970526     C*------------------------------------------------------------------------*
012000970526     C     SNDMSG        BEGSR
012100970526     C*
012200970526     C* PULISCE IL SFILE MESSAGGI
012300970526     C                   MOVEL     '1'           *IN40
012400020415     C                   WRITE     SIT6DMC
012500970526     C                   MOVEL     '0'           *IN40
012600970526     C*
012700970526     C* VISUALIZZA IL MESSAGGIO
012800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012900970526     C                   CALL      'XMSG'        XMSGPA
013000970526IF  1C     DSMSER        IFEQ      *ZEROS
013100970526     C                   MOVEL     DSMSKE        MSGKEY
013200970526E   1C                   ENDIF
013300970526     C                   Z-ADD     1             MSGULT
013400020415     C                   WRITE     SIT6DMS
013500020415     C                   WRITE     SIT6DMC
013600970526     C*
013700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013800970526     C                   MOVEL     *BLANKS       DSMSMS
013900970526     C*
014000970526     C                   ENDSR
014100970526     C*------------------------------------------------------------------------*
014200970526     C* ENTGES - GESTIONE ENTER -> INVIO
014300970526     C*------------------------------------------------------------------------*
014400970526     C     ENTGES        BEGSR
014500970526     C*
014600020415     C                   EXSR      TEST
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* F03GES - GESTIONE F03 -> FINE
015100970526     C*------------------------------------------------------------------------*
015200970526     C     F03GES        BEGSR
015300970526     C*
015400970526     C* IMPOSTA "PREMUTO F03"
015500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F06GES        BEGSR
016200970526     C*
016300020412     C*
016400020412     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* F12GES - GESTIONE F12 -> RITORNO
016700970526     C*------------------------------------------------------------------------*
016800970526     C     F12GES        BEGSR
016900970526     C*
017000970526     C* IMPOSTA "PREMUTO F12"
017100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017600970526     C*------------------------------------------------------------------------*
017700970526     C     F24GES        BEGSR
017800970526     C*
017900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300051227     C* TEST - OPERAZIONI DI TEST X REPERIMENTO DESTINATARIO PARTICOLARE
018400970526     C*------------------------------------------------------------------------*
018500020415     C     TEST          BEGSR
018600970526     C*
018700020415     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
018800020415     C                   EXSR      IMPCAM
018900061116     C*
019000061116     C* Salvo il timestamp prima d eseguire la ricerca
019100061116     C                   TIME                    wTimeStr
019200970526     C*
019300020415     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
019400040319     C                   EVAL      IT0TLA = 'P'
019500020415     C                   CALL      'TISIT5R'
019600020415     C                   PARM                    TISIT0DS
019700070904     C                   PARM                    TISIT0X1DS
019800061116     C*
019900061116     C* Salvo il timestamp dopo aver eseguito la ricerca
020000061116     C                   TIME                    wTimeEnd
020100020415     C*
020200020415     C* IMPOSTO A VIDEO I CAMPI RESTITUITI NELLA DS DI PASSAGGIO DI PROCEDURA
020300020415     C                   EVAL      VODDOS = OT0DOS
020400051227     C* REPERISCO LA DESCRIZIONE DEL TIPO DESTINATARIO PARTICOLARE DA TABELLA ("TDP")
020500051227     C                   EVAL      tbeCOD = 'TDP'
020600051227     C                   EVAL      tbeKE1 = OT0DOS
020700051227     C     KEYtbe01_P    CHAIN     tntbe01l
020800051227     C                   IF        %found(tntbe01l)
020900051227     C                   EVAL      DTDP = tbeUNI
021000051227     C                   EVAL      VDDTDP = §TDPDES
021100051227     C                   ENDIF
021200051227     C*
021300020415     C                   EVAL      VODLIV = OT0LIV
021400020415     C                   EVAL      VODERR = OT0ERR
021500020415     C                   EVAL      VODMSG = OT0MSG
021600020415     C                   EVAL      VODNAZ = OT0NAZ
021700020415     C                   EVAL      VODPRV = OT0PRV
021800020415     C                   EVAL      VODCAP = OT0CAP
021900020415     C                   EVAL      VODLOC = OT0LOC
022000020415     C                   EVAL      VODIND = OT0IND
022100020415     C                   EVAL      VODRAG = OT0RAG
022200060509     C                   MOVE(P)   OT0COD        VODCOD
022300020415     C                   EVAL      VODSIN = OT0SIN
022400040601     C                   EVAL      VODTEL = OT0TEL
022500040601     C                   EVAL      VODREF = OT0REF
022600051227     C                   EVAL      VODFLO = OT0FLO
022700070904     C*
022800070904     C* Imposto i dati d estensione (primi 55 byte ds TISIT0X1DS)
022900070904     C                   MOVEL(P)  TISIT0X1DS    VODT0X1
023000061116     C*
023100061116     C* Se richiesto debug performance visualizzo i risultati
023200061116     C                   IF        VIDDBG = 'S'
023300061116     C     wTimeStr      DSPLY
023400061116     C     wTimeEnd      DSPLY
023500061116     C                   ENDIF
023600020412     C*
023700020412     C                   ENDSR
023800000407     C*------------------------------------------------------------------------*
023900020415     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
024000000407     C*------------------------------------------------------------------------*
024100000407     C     IMPCAM        BEGSR
024200000407     C*
024300020415     C                   MOVEL     VIDRAG        IT0RAG
024400020415     C                   MOVEL     VIDNAZ        IT0NAZ
024500020415     C                   MOVEL     VIDPRV        IT0PRV
024600020415     C                   MOVEL     VIDCAP        IT0CAP
024700020415     C                   MOVEL     VIDLOC        IT0LOC
024800020416     C                   MOVEL     VIDIND        IT0IND
024900070904     C*
025000070904     C                   CLEAR                   TISIT0X1DS
025100000407     C*
025200000407     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025500970526     C*------------------------------------------------------------------------*
025600970526     C     IMPFUN        BEGSR
025700970526     C*
025800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
025900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
026000970526     C                   EXSR      FUNGES
026100970526     C*
026200970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
026300970526     C                   MOVEL     DEC(1)        VIDFUN
026400970526     C*
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700970526     C* *INZSR - OPERAZIONI INIZIALI
026800970526     C*------------------------------------------------------------------------*
026900970526     C     *INZSR        BEGSR
027000970526     C*---
027100970526     C* RICEVIMENTO PARAMETRI
027200970526     C*---
027300970526     C     *ENTRY        PLIST
027400970612     C                   PARM                    KPJBA
027500051227     C*
027600051227     C* DEFINIZIONE CHIAVI
027700051227     C     KEYtbe01_P    KLIST
027800051227     C                   KFLD                    tbeCOD
027900051227     C                   KFLD                    tbeKE1
028000970526     C*
028100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
028200970526     C     XMSGPA        PLIST
028300970526     C                   PARM                    DSMSMS
028400970526     C                   PARM                    DSMSPG
028500970526     C                   PARM                    DSMSKE
028600970526     C                   PARM                    DSMSER
028700970526     C*
028800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
028900970526     C                   CALL      'XKEY'
029000970526     C                   PARM                    DSKEY
029100970526     C*---
029200970526     C* VARIABILI NON RIFERITE AL DATA BASE
029300970526     C*---
029400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
029500970526     C*---
029600970526     C* VARIABILI DI CONTROLLO
029700970526     C*---
029800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
029900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
030000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
030100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
030200970526     C*
030300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
030400020415     C                   MOVEL     'TISIT6R'     DSMSPG
030500020415     C                   MOVEL     'TISIT6R'     MSGQUE
030600970526     C                   MOVEL     *BLANKS       MSGKEY
030700970526     C                   MOVEL     *BLANKS       DSMSMS
030800061116     C*
030900061116     C* INIZIALIZZAZIONE VIDEO
031000061116     C                   EVAL      VIDDBG = 'N'
031100970526     C*
031200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
031300970526     C                   EXSR      IMPFUN
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
031800970526     C*------------------------------------------------------------------------*
031900970526     C     FUNGES        BEGSR
032000970526     C*
032100970526     C* PULISCO CAMPI DI LAVORO
032200970526     C                   Z-ADD     0             W                 5 0
032300970526     C*
032400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
032500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
032600970526     C     1             DO        24            I
032700970526     C                   MOVEL     FUD(I)        FUDRIG
032800970526     C*
032900970526     C* PULIZIA FUNZIONI PRECEDENTI
033000970526     C                   MOVEL     *BLANKS       FUV(I)
033100970526     C*
033200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
033300970526     C* (SALTA F24)
033400970526     C     FUDATT        IFEQ      'S'
033500970526     C     FUDFUN        ANDNE     '24'
033600970526     C                   ADD       1             W
033700970526     C                   MOVEL     FUDDES        FUV(W)
033800970526     C                   END
033900970526     C                   END                                                    DO WHILE
034000970526     C*
034100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
034200970526     C* ALTRIMENTI DISATTIVO F24
034300970526     C     1             DO        24            I
034400970526     C                   MOVEL     FUD(I)        FUDRIG
034500970526     C*
034600970526     C     FUDFUN        IFEQ      '24'
034700970526     C     W             IFGT      6
034800970526     C                   MOVEL     'S'           FUDATT
034900970526     C                   ELSE
035000970526     C                   MOVEL     *BLANKS       FUDATT
035100970526     C                   END                                                    W GT 8
035200970526     C                   MOVEL     FUDRIG        FUD(I)
035300970526     C                   END
035400970526     C                   END                                                    DO WHILE
035500970526     C*
035600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
035700970526     C     W             DIV       6             I
035800970526     C                   MVR                     Z
035900970526     C*
036000970526     C     Z             IFNE      0
036100970526     C     I             ADD       1             Z
036200970526     C                   ELSE
036300970526     C                   Z-ADD     I             Z                 5 0
036400970526     C                   END
036500970526     C*
036600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
036700970526     C                   ADD       1             FUNGRU
036800970526     C     FUNGRU        IFGT      Z
036900970526     C     FUNGRU        ORLT      1
037000970526     C                   Z-ADD     1             FUNGRU
037100970526     C                   END
037200970526     C*
037300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
037400970526     C     FUNGRU        IFEQ      1
037500970526     C                   MOVEL     FUV(1)        FUN001
037600970526     C                   MOVEL     FUV(2)        FUN002
037700970526     C                   MOVEL     FUV(3)        FUN003
037800970526     C                   MOVEL     FUV(4)        FUN004
037900970526     C                   MOVEL     FUV(5)        FUN005
038000970526     C                   MOVEL     FUV(6)        FUN006
038100970526     C                   END
038200970526     C*
038300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
038400970526     C     FUNGRU        IFEQ      2
038500970526     C                   MOVEL     FUV(7)        FUN001
038600970526     C                   MOVEL     FUV(8)        FUN002
038700970526     C                   MOVEL     FUV(9)        FUN003
038800970526     C                   MOVEL     FUV(10)       FUN004
038900970526     C                   MOVEL     FUV(11)       FUN005
039000970526     C                   MOVEL     FUV(12)       FUN006
039100970526     C                   END
039200970526     C*
039300970526     C* TERZO GRUPPO DI 6 FUNZIONI
039400970526     C     FUNGRU        IFEQ      3
039500970526     C                   MOVEL     FUV(13)       FUN001
039600970526     C                   MOVEL     FUV(14)       FUN002
039700970526     C                   MOVEL     FUV(15)       FUN003
039800970526     C                   MOVEL     FUV(16)       FUN004
039900970526     C                   MOVEL     FUV(17)       FUN005
040000970526     C                   MOVEL     FUV(18)       FUN006
040100970526     C                   END
040200970526     C*
040300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
040400970526     C     FUNGRU        IFEQ      4
040500970526     C                   MOVEL     FUV(19)       FUN001
040600970526     C                   MOVEL     FUV(20)       FUN002
040700970526     C                   MOVEL     FUV(21)       FUN003
040800970526     C                   MOVEL     FUV(22)       FUN004
040900970526     C                   MOVEL     *BLANKS       FUN005
041000970526     C                   MOVEL     *BLANKS       FUN006
041100970526     C                   END
041200970526     C*
041300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
041400970526     C                   MOVEL     *BLANKS       FUN007
041500970526     C                   MOVEL     *BLANKS       FUN008
041600970526     C*
041700970526     C     1             DO        24            I
041800970526     C                   MOVEL     FUD(I)        FUDRIG
041900970526     C     FUDFUN        IFEQ      '24'
042000970526     C     FUDATT        ANDEQ     'S'
042100970526     C                   MOVEL     FUDDES        FUN008
042200970526     C                   END
042300970526     C                   END                                                    DO
042400970526     C*
042500970526     C                   ENDSR
042600970526     C*------------------------------------------------------------------------*
042700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
042800970526     C*------------------------------------------------------------------------*
042900970526     C     FUNCON        BEGSR
043000970526     C*
043100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
043200970526     C                   EXSR      FUNVAR
043300970526     C*
043400970526     C* PULISCO CAMPI DI LAVORO
043500970526     C                   Z-ADD     0             I                 5 0
043600970526     C                   Z-ADD     0             W                 5 0
043700970526     C*
043800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
043900970526     C     FUNKEY        IFNE      *BLANKS
044000970526     C     1             DO        24            I
044100970526     C                   MOVEL     FUD(I)        FUDRIG
044200970526     C*
044300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
044400970526     C     FUNKEY        IFEQ      FUDFUN
044500970526     C     FUDATT        ANDEQ     'S'
044600970526     C                   Z-ADD     I             W
044700970526     C                   END
044800970526     C                   END                                                    DO WHILE
044900970526     C*
045000970526     C* FUNZIONE NON ATTIVA (ERRORE)
045100970526     C     W             IFEQ      0
045200970526     C*
045300970526     C* EMISSIONE MESSAGGIO DI ERRORE
045400970526     C                   MOVEL     MSG(1)        DSMSMS
045500970526     C*
045600970526     C                   END                                                    W=0
045700970526     C                   END                                                    FUNKEY NE *BLANKS
045800970526     C*
045900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
046000970526     C     W             IFNE      0
046100970526     C     FUNKEY        OREQ      *BLANKS
046200970526     C     FUNKEY        IFEQ      *BLANKS
046300970526     C                   EXSR      ENTGES
046400970526     C                   END
046500970526     C     FUNKEY        IFEQ      '03'
046600970526     C                   EXSR      F03GES
046700970526     C                   END
046800970526     C     FUNKEY        IFEQ      '06'
046900970526     C                   EXSR      F06GES
047000970526     C                   END
047100970526     C     FUNKEY        IFEQ      '12'
047200970526     C                   EXSR      F12GES
047300970526     C                   END
047400970526     C     FUNKEY        IFEQ      '24'
047500970526     C                   EXSR      F24GES
047600970526     C                   END
047700970526     C                   END
047800970526     C*
047900970526     C                   ENDSR
048000970526     C*------------------------------------------------------------------------*
048100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
048200970526     C*------------------------------------------------------------------------*
048300970526     C     FUNVAR        BEGSR
048400970526     C*
048500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
048600970526     C*
048700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
048800970526     C     KEY           IFEQ      ENTER
048900970526     C                   MOVEL     *BLANKS       FUNKEY
049000970526     C                   END
049100970526     C     KEY           IFEQ      F1
049200970526     C                   MOVEL     '01'          FUNKEY
049300970526     C                   END
049400970526     C     KEY           IFEQ      F2
049500970526     C                   MOVEL     '02'          FUNKEY
049600970526     C                   END
049700970526     C     KEY           IFEQ      F3
049800970526     C                   MOVEL     '03'          FUNKEY
049900970526     C                   END
050000970526     C     KEY           IFEQ      F4
050100970526     C                   MOVEL     '04'          FUNKEY
050200970526     C                   END
050300970526     C     KEY           IFEQ      F5
050400970526     C                   MOVEL     '05'          FUNKEY
050500970526     C                   END
050600970526     C     KEY           IFEQ      F6
050700970526     C                   MOVEL     '06'          FUNKEY
050800970526     C                   END
050900970526     C     KEY           IFEQ      F7
051000970526     C                   MOVEL     '07'          FUNKEY
051100970526     C                   END
051200970526     C     KEY           IFEQ      F8
051300970526     C                   MOVEL     '08'          FUNKEY
051400970526     C                   END
051500970526     C     KEY           IFEQ      F9
051600970526     C                   MOVEL     '09'          FUNKEY
051700970526     C                   END
051800970526     C     KEY           IFEQ      F10
051900970526     C                   MOVEL     '10'          FUNKEY
052000970526     C                   END
052100970526     C     KEY           IFEQ      F11
052200970526     C                   MOVEL     '11'          FUNKEY
052300970526     C                   END
052400970526     C     KEY           IFEQ      F12
052500970526     C                   MOVEL     '12'          FUNKEY
052600970526     C                   END
052700970526     C     KEY           IFEQ      F13
052800970526     C                   MOVEL     '13'          FUNKEY
052900970526     C                   END
053000970526     C     KEY           IFEQ      F14
053100970526     C                   MOVEL     '14'          FUNKEY
053200970526     C                   END
053300970526     C     KEY           IFEQ      F15
053400970526     C                   MOVEL     '15'          FUNKEY
053500970526     C                   END
053600970526     C     KEY           IFEQ      F16
053700970526     C                   MOVEL     '16'          FUNKEY
053800970526     C                   END
053900970526     C     KEY           IFEQ      F17
054000970526     C                   MOVEL     '17'          FUNKEY
054100970526     C                   END
054200970526     C     KEY           IFEQ      F18
054300970526     C                   MOVEL     '18'          FUNKEY
054400970526     C                   END
054500970526     C     KEY           IFEQ      F19
054600970526     C                   MOVEL     '19'          FUNKEY
054700970526     C                   END
054800970526     C     KEY           IFEQ      F20
054900970526     C                   MOVEL     '20'          FUNKEY
055000970526     C                   END
055100970526     C     KEY           IFEQ      F21
055200970526     C                   MOVEL     '21'          FUNKEY
055300970526     C                   END
055400970526     C     KEY           IFEQ      F22
055500970526     C                   MOVEL     '22'          FUNKEY
055600970526     C                   END
055700970526     C     KEY           IFEQ      F23
055800970526     C                   MOVEL     '23'          FUNKEY
055900970526     C                   END
056000970526     C     KEY           IFEQ      F24
056100970526     C                   MOVEL     '24'          FUNKEY
056200970526     C                   END
056300970526     C     KEY           IFEQ      ROLLUP
056400970526     C                   MOVEL     'RU'          FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      HELP
056700970526     C                   MOVEL     'HP'          FUNKEY
056800970526     C                   END
056900970526     C                   ENDSR
057000970526     C*------------------------------------------------------------------------*
057100970526     C* NOTE COSTRUZIONE SCHIERE
057200970526     C*------------------------------------------------------------------------*
057300970526     C*
057400970526     C* -MSG-MESSAGGI
057500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
057600970526     C* (NON TOCCARE MSG,1 E MSG,2)
057700970526     C*
057800970526     C* -CMD-COMANDI
057900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
058000970526     C*
058100970526     C* -OPD-OPZIONI
058200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
058300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
058400970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
058500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
058600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
058700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
058800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
058900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
059000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
059100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
059200970526     C*
059300970526     C* -FUD-FUNZIONI
059400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
059500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
059600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
059700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
059800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
059900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
060000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
060100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
060200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
060300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
060400970526     O*------------------------------------------------------------------------*
060500970526** DEC - DECODIFICHE
060600020415     TEST                                                         1
060700020415                                                                  2
060800020415                                                                  3
060900020415                                                                  4
061000020415                                                                  5
061100020415                                                                  6
061200970528** MSG - MESSAGGI
061300020415TISIT6R- Tasto funzionale non gestito                                          1
061400020415TISIT6R- Inserimento effettuato                                                2
061500020415TISIT6R- Modifica effettuata                                                   3
061600020415TISIT6R- Annullamento effettuato                                               4
061700020415TISIT6R- Ripristino effettuato                                                 5
061800020415TISIT6R- Richiesta funzione per record inesistente.                            6
061900020415TISIT6R- Impossibile inserire, record già esistente.                           7
062000020415TISIT6R- Richiesta funzione per record esistente                               8
062100020415TISIT6R- Impossibile indicare '*' nella Ragione Sociale, correggere.           9
062200020415TISIT6R- Impossibile indicare il sinonimo uguale alla Ragione Sociale.         10
062300020415TISIT6R- Per indicare qualsiasi valore inserire solo '*', correggere.          11
062400020415TISIT6R- Tutti i campi sono obbligatori                                        12
062500020415TISIT6R- Il sinonimo non si riferisce a nessun destinatario dis./sup.          13
062600020415TISIT6R- Impossibile indicare '*' nel sinonimo, correggere.                    14
062700020415TISIT6R- Copia effettuata                                                      15
062800020415TISIT6R- ATTENZIONE... dati a video sostituiti !                               16
062900020415TISIT6R- NAZ/PRV/CAP/LOC incongruenti, verificare e correggere.                17
063000020415                                                                               18
063100020415                                                                               19
063200020415                                                                               20
063300970526** FUD - FUNZIONI
063400970526                   -        (S/N)=N                        funz.01¬
063500970526                   -        (S/N)=N                        funz.02¬
063600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
063700970526                   -        (S/N)=N                        funz.04¬
063800970526                   -        (S/N)=N                        funz.05¬
063900061116F06=Conferma       -        (S/N)=N                        funz.06¬
064000970527                   -        (S/N)=N                        funz.07¬
064100970527                   -        (S/N)=N                        funz.08¬
064200970527                   -        (S/N)=N                        funz.09¬
064300970527                   -        (S/N)=N                        funz.10¬
064400970526                   -        (S/N)=N                        funz.11¬
064500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
064600970526                   -        (S/N)=N                        funz.13¬
064700970526                   -        (S/N)=N                        funz.14¬
064800970526                   -        (S/N)=N                        funz.15¬
064900970526                   -        (S/N)=N                        funz.16¬
065000970526                   -        (S/N)=N                        funz.17¬
065100970526                   -        (S/N)=N                        funz.18¬
065200970526                   -        (S/N)=N                        funz.19¬
065300970526                   -        (S/N)=N                        funz.20¬
065400970526                   -        (S/N)=N                        funz.21¬
065500970526                   -        (S/N)=N                        funz.22¬
065600970526                   -        (S/N)=N                        funz.23¬
065700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

000100970526     H*------------------------------------------------------------------------*
000200030808     H* CREA WRK-FILE VARIE FATTURATE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030916     FTIST71D   CF   E             WORKSTN INFDS(DEVDS1)
000900030916     F                                     SFILE(ST71DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D*------------
001400970526     D* MESSAGGI
001500970526     D*------------
001600970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* DESCRIZIONI TASTI FUNZIONALI
001900970526     D*------------
002000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* FUNZIONI DA CARICARE A VIDEO
002300970526     D*------------
002400970526     D FUV             S             19    DIM(24)
002500970526     D*------------------------------------------------------------------------*
002600970526     D* INPUT
002700970526     D*------------------------------------------------------------------------*
002800970526     D*------------
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000970526     D*------------
005100020415     D*------------
005200970526     D* ARCHITETTURA
005300970526     D*------------
005400970526     D KPJBA         E DS
005500000407     D*------------------
005600030626     D* DS X PASSAGGIO PARAMETRI
005700020412     D*------------------
005800030808     D PARAM           DS
005900030916     D  MMAAAAFAT              1      6
006000030916     D  MMAAAAFATNUM           1      6  0
006100030916     D  MMFAT                  1      2  0
006200030916     D  AAAAFAT                3      6  0
006300030916     D  DATAFATFINMES          7     14  0
006400030916     D  GGFATFINMES            7      8  0
006500030916     D  MMFATFINMES            9     10  0
006600030916     D  AAAAFATFINMES         11     14  0
006700030916     D  DETTAGLIO             15     15
006800031111     D  SOLOVARIA             16     16
006900051227     D  PERIODOCEAAAA         17     20  0
007000051227     D  PERIODOCEMM           21     22  0
007100051227     D  PERIODOCE             17     22  0
007200051227     D  STORNONACC            23     23
007300051227     D  ELABORA_CE            24     24
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
010200030916     C                   WRITE     ST71D01
010300970526     C*
010400970526     C* SPEDISCE EVENTUALE MESSAGGIO
010500970526     C                   EXSR      SNDMSG
010600970526     C*
010700970526     C* EMETTE LE FUNZIONI
010800030916     C                   WRITE     ST71D02
010900970526     C*
011000970526     C* EMETTE IL VIDEO
011100030916     C                   EXFMT     ST71D03
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
012400030916     C                   WRITE     ST71DMC
012500970526     C                   MOVEL     '0'           *IN40
012600970526     C*
012700970526     C* VISUALIZZA IL MESSAGGIO
012800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012900970526     C                   CALL      'XMSG'        XMSGPA
013000970526IF  1C     DSMSER        IFEQ      *ZEROS
013100970526     C                   MOVEL     DSMSKE        MSGKEY
013200970526E   1C                   ENDIF
013300970526     C                   Z-ADD     1             MSGULT
013400030916     C                   WRITE     ST71DMS
013500030916     C                   WRITE     ST71DMC
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
014600051229     C* ESEGUO CONTROLLI FORMALI
014700051229     C                   EXSR      DECODI
014800970526     C*
014900970526     C                   ENDSR
015000970526     C*------------------------------------------------------------------------*
015100970526     C* F03GES - GESTIONE F03 -> FINE
015200970526     C*------------------------------------------------------------------------*
015300970526     C     F03GES        BEGSR
015400970526     C*
015500970526     C* IMPOSTA "PREMUTO F03"
015600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015700970526     C*
015800970526     C                   ENDSR
015900030808     C*------------------------------------------------------------------------*
016000030808     C* F06GES - GESTIONE F06 -> LANCIO ELABORAZIONE
016100030808     C*------------------------------------------------------------------------*
016200030808     C     F06GES        BEGSR
016300051229     C*
016400051229     C* ESEGUO CONTROLLI FORMALI
016500051229     C                   EXSR      DECODI
016600051229     C*
016700051229     C* SE TUTTO OK => PROCEDO
016800051229     C                   IF        ERRL00 = '0'
016900030808     C*
017000030808     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
017100030808     C                   EXSR      IMPCAM
017200030808     C*
017300030808     C* LANCIO ELABORAZIONE
017400030916     C                   MOVEL     'ST72'        KCOAZ
017500030808     C                   MOVEL(P)  PARAM         KPJBU
017600051229     C                   CALL      'BCH10'
017700051229 MOM C***                CALL      'TIST72C'
017800030808     C                   PARM                    KPJBA
017900030811     C
018000030808     C*
018100030808     C* IMPOSTA USCITA PGM
018200030808     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018300051229     C*
018400051229     C                   ENDIF
018500030808     C*
018600030808     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F12GES - GESTIONE F12 -> RITORNO
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F12GES        BEGSR
019100970526     C*
019200970526     C* IMPOSTA "PREMUTO F12"
019300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F24GES        BEGSR
020000970526     C*
020100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020200970526     C*
020300970526     C                   ENDSR
020400051229     C*------------------------------------------------------------------------*
020500051229     C* DECODI - CONTROLLI FORMALI
020600051229     C*------------------------------------------------------------------------*
020700051229     C     DECODI        BEGSR
020800051229     C*
020900051229     C* INIZIALIZZO INDICATORI DI ERRORE
021000051229     C                   SETOFF                                       8081
021100051229     C*
021200051229     C* RESETTO FLAG DI ERRORE
021300051229     C                   MOVEL     '0'           ERRL00
021400051229     C*
021500051229     C* OBBLIGATORIETA' CAMPI
021600051229     C                   IF        ERRL00 = '0'
021700051229     C                   IF        VIDAMF = *zeros
021800051229     C                   SETON                                        80
021900051229     C                   MOVEL     '1'           ERRL00
022000051229     C                   MOVEL     MSG(2)        DSMSMS
022100051229     C                   ENDIF
022200051229     C                   ENDIF
022300051229     C*
022400051229     C                   IF        ERRL00 = '0'
022500051229     C                   IF        VIDCE  = 'S'
022600051229     C                   IF        VIDPER = *zeros
022700051229     C                   SETON                                        81
022800051229     C                   MOVEL     '1'           ERRL00
022900051229     C                   MOVEL     MSG(2)        DSMSMS
023000051229     C                   ENDIF
023100051229     C                   ENDIF
023200051229     C                   ENDIF
023300051229     C*
023400051229     C                   ENDSR
023500000407     C*------------------------------------------------------------------------*
023600051227     C* IMPCAM - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
023700000407     C*------------------------------------------------------------------------*
023800000407     C     IMPCAM        BEGSR
023900000407     C*
024000030916     C                   Z-ADD     VIDAMF        MMAAAAFATNUM
024100030916     C                   IF        MMFAT = 12
024200030916     C                   EVAL      DATAFATFINMES = 0101*10000+AAAAFAT+1
024300030916     C                   ELSE
024400030916     C                   EVAL      DATAFATFINMES = 01*1000000+(MMFAT+1)*10000+
024500030916     C                                             AAAAFAT
024600030916     C                   ENDIF
024700030916     C                   MOVEL     VIDDET        DETTAGLIO
024800031111     C                   MOVEL     VIDVAR        SOLOVARIA
024900051227     C                   MOVEL     VIDCE         ELABORA_CE
025000051227     C                   MOVEL     VIDSTN        STORNONACC
025100051227     C*
025200051227     C* INVERTO IL PERIODO C.E.
025300051227     C                   MOVE(P)   VIDPER        PERIODOCEAAAA
025400051227     C                   MOVEL(P)  VIDPER        PERIODOCEMM
025500000407     C*
025600000407     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025900970526     C*------------------------------------------------------------------------*
026000970526     C     IMPFUN        BEGSR
026100970526     C*
026200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
026300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
026400970526     C                   EXSR      FUNGES
026500970526     C*
026600970526     C                   ENDSR
026700970526     C*------------------------------------------------------------------------*
026800030626     C* *INZSR - OPERAZIONI INIZIALI
026900970526     C*------------------------------------------------------------------------*
027000970526     C     *INZSR        BEGSR
027100970526     C*---
027200970526     C* RICEVIMENTO PARAMETRI
027300970526     C*---
027400970526     C     *ENTRY        PLIST
027500970612     C                   PARM                    KPJBA
027600970526     C*
027700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
027800970526     C     XMSGPA        PLIST
027900970526     C                   PARM                    DSMSMS
028000970526     C                   PARM                    DSMSPG
028100970526     C                   PARM                    DSMSKE
028200970526     C                   PARM                    DSMSER
028300970526     C*
028400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
028500970526     C                   CALL      'XKEY'
028600970526     C                   PARM                    DSKEY
028700970526     C*---
028800970526     C* VARIABILI NON RIFERITE AL DATA BASE
028900970526     C*---
029000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
029100970526     C*---
029200970526     C* VARIABILI DI CONTROLLO
029300970526     C*---
029400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
029500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
029600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
029700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
029800051227     C*
029900051227     C* INIZIALIZZAZIONE CAMPI VIDEO
030000051227     C                   MOVEL     'N'           VIDDET
030100051227     C                   MOVEL     'N'           VIDCE
030200051227     C                   MOVEL     'N'           VIDSTN
030300970526     C*
030400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
030500030916     C                   MOVEL     'TIST71R'     DSMSPG
030600030916     C                   MOVEL     'TIST71R'     MSGQUE
030700970526     C                   MOVEL     *BLANKS       MSGKEY
030800970526     C                   MOVEL     *BLANKS       DSMSMS
030900970526     C*
031000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
031100970526     C                   EXSR      IMPFUN
031200970526     C*
031300970526     C                   ENDSR
031400970526     C*------------------------------------------------------------------------*
031500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
031600970526     C*------------------------------------------------------------------------*
031700970526     C     FUNGES        BEGSR
031800970526     C*
031900970526     C* PULISCO CAMPI DI LAVORO
032000970526     C                   Z-ADD     0             W                 5 0
032100970526     C*
032200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
032300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
032400970526     C     1             DO        24            I
032500970526     C                   MOVEL     FUD(I)        FUDRIG
032600970526     C*
032700970526     C* PULIZIA FUNZIONI PRECEDENTI
032800970526     C                   MOVEL     *BLANKS       FUV(I)
032900970526     C*
033000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
033100970526     C* (SALTA F24)
033200970526     C     FUDATT        IFEQ      'S'
033300970526     C     FUDFUN        ANDNE     '24'
033400970526     C                   ADD       1             W
033500970526     C                   MOVEL     FUDDES        FUV(W)
033600970526     C                   END
033700970526     C                   END                                                    DO WHILE
033800970526     C*
033900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
034000970526     C* ALTRIMENTI DISATTIVO F24
034100970526     C     1             DO        24            I
034200970526     C                   MOVEL     FUD(I)        FUDRIG
034300970526     C*
034400970526     C     FUDFUN        IFEQ      '24'
034500970526     C     W             IFGT      6
034600970526     C                   MOVEL     'S'           FUDATT
034700970526     C                   ELSE
034800970526     C                   MOVEL     *BLANKS       FUDATT
034900970526     C                   END                                                    W GT 8
035000970526     C                   MOVEL     FUDRIG        FUD(I)
035100970526     C                   END
035200970526     C                   END                                                    DO WHILE
035300970526     C*
035400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
035500970526     C     W             DIV       6             I
035600970526     C                   MVR                     Z
035700970526     C*
035800970526     C     Z             IFNE      0
035900970526     C     I             ADD       1             Z
036000970526     C                   ELSE
036100970526     C                   Z-ADD     I             Z                 5 0
036200970526     C                   END
036300970526     C*
036400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
036500970526     C                   ADD       1             FUNGRU
036600970526     C     FUNGRU        IFGT      Z
036700970526     C     FUNGRU        ORLT      1
036800970526     C                   Z-ADD     1             FUNGRU
036900970526     C                   END
037000970526     C*
037100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
037200970526     C     FUNGRU        IFEQ      1
037300970526     C                   MOVEL     FUV(1)        FUN001
037400970526     C                   MOVEL     FUV(2)        FUN002
037500970526     C                   MOVEL     FUV(3)        FUN003
037600970526     C                   MOVEL     FUV(4)        FUN004
037700970526     C                   MOVEL     FUV(5)        FUN005
037800970526     C                   MOVEL     FUV(6)        FUN006
037900970526     C                   END
038000970526     C*
038100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
038200970526     C     FUNGRU        IFEQ      2
038300970526     C                   MOVEL     FUV(7)        FUN001
038400970526     C                   MOVEL     FUV(8)        FUN002
038500970526     C                   MOVEL     FUV(9)        FUN003
038600970526     C                   MOVEL     FUV(10)       FUN004
038700970526     C                   MOVEL     FUV(11)       FUN005
038800970526     C                   MOVEL     FUV(12)       FUN006
038900970526     C                   END
039000970526     C*
039100970526     C* TERZO GRUPPO DI 6 FUNZIONI
039200970526     C     FUNGRU        IFEQ      3
039300970526     C                   MOVEL     FUV(13)       FUN001
039400970526     C                   MOVEL     FUV(14)       FUN002
039500970526     C                   MOVEL     FUV(15)       FUN003
039600970526     C                   MOVEL     FUV(16)       FUN004
039700970526     C                   MOVEL     FUV(17)       FUN005
039800970526     C                   MOVEL     FUV(18)       FUN006
039900970526     C                   END
040000970526     C*
040100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
040200970526     C     FUNGRU        IFEQ      4
040300970526     C                   MOVEL     FUV(19)       FUN001
040400970526     C                   MOVEL     FUV(20)       FUN002
040500970526     C                   MOVEL     FUV(21)       FUN003
040600970526     C                   MOVEL     FUV(22)       FUN004
040700970526     C                   MOVEL     *BLANKS       FUN005
040800970526     C                   MOVEL     *BLANKS       FUN006
040900970526     C                   END
041000970526     C*
041100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
041200970526     C                   MOVEL     *BLANKS       FUN007
041300970526     C                   MOVEL     *BLANKS       FUN008
041400970526     C*
041500970526     C     1             DO        24            I
041600970526     C                   MOVEL     FUD(I)        FUDRIG
041700970526     C     FUDFUN        IFEQ      '24'
041800970526     C     FUDATT        ANDEQ     'S'
041900970526     C                   MOVEL     FUDDES        FUN008
042000970526     C                   END
042100970526     C                   END                                                    DO
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
042600970526     C*------------------------------------------------------------------------*
042700970526     C     FUNCON        BEGSR
042800970526     C*
042900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
043000970526     C                   EXSR      FUNVAR
043100970526     C*
043200970526     C* PULISCO CAMPI DI LAVORO
043300970526     C                   Z-ADD     0             I                 5 0
043400970526     C                   Z-ADD     0             W                 5 0
043500970526     C*
043600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
043700970526     C     FUNKEY        IFNE      *BLANKS
043800970526     C     1             DO        24            I
043900970526     C                   MOVEL     FUD(I)        FUDRIG
044000970526     C*
044100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
044200970526     C     FUNKEY        IFEQ      FUDFUN
044300970526     C     FUDATT        ANDEQ     'S'
044400970526     C                   Z-ADD     I             W
044500970526     C                   END
044600970526     C                   END                                                    DO WHILE
044700970526     C*
044800970526     C* FUNZIONE NON ATTIVA (ERRORE)
044900970526     C     W             IFEQ      0
045000970526     C*
045100970526     C* EMISSIONE MESSAGGIO DI ERRORE
045200970526     C                   MOVEL     MSG(1)        DSMSMS
045300970526     C*
045400970526     C                   END                                                    W=0
045500970526     C                   END                                                    FUNKEY NE *BLANKS
045600970526     C*
045700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
045800970526     C     W             IFNE      0
045900970526     C     FUNKEY        OREQ      *BLANKS
046000970526     C     FUNKEY        IFEQ      *BLANKS
046100970526     C                   EXSR      ENTGES
046200970526     C                   END
046300970526     C     FUNKEY        IFEQ      '03'
046400970526     C                   EXSR      F03GES
046500970526     C                   END
046600030808     C     FUNKEY        IFEQ      '06'
046700030808     C                   EXSR      F06GES
046800030808     C                   END
046900970526     C     FUNKEY        IFEQ      '12'
047000970526     C                   EXSR      F12GES
047100970526     C                   END
047200970526     C     FUNKEY        IFEQ      '24'
047300970526     C                   EXSR      F24GES
047400970526     C                   END
047500970526     C                   END
047600970526     C*
047700970526     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
048000970526     C*------------------------------------------------------------------------*
048100970526     C     FUNVAR        BEGSR
048200970526     C*
048300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
048400970526     C*
048500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
048600970526     C     KEY           IFEQ      ENTER
048700970526     C                   MOVEL     *BLANKS       FUNKEY
048800970526     C                   END
048900970526     C     KEY           IFEQ      F1
049000970526     C                   MOVEL     '01'          FUNKEY
049100970526     C                   END
049200970526     C     KEY           IFEQ      F2
049300970526     C                   MOVEL     '02'          FUNKEY
049400970526     C                   END
049500970526     C     KEY           IFEQ      F3
049600970526     C                   MOVEL     '03'          FUNKEY
049700970526     C                   END
049800970526     C     KEY           IFEQ      F4
049900970526     C                   MOVEL     '04'          FUNKEY
050000970526     C                   END
050100970526     C     KEY           IFEQ      F5
050200970526     C                   MOVEL     '05'          FUNKEY
050300970526     C                   END
050400970526     C     KEY           IFEQ      F6
050500970526     C                   MOVEL     '06'          FUNKEY
050600970526     C                   END
050700970526     C     KEY           IFEQ      F7
050800970526     C                   MOVEL     '07'          FUNKEY
050900970526     C                   END
051000970526     C     KEY           IFEQ      F8
051100970526     C                   MOVEL     '08'          FUNKEY
051200970526     C                   END
051300970526     C     KEY           IFEQ      F9
051400970526     C                   MOVEL     '09'          FUNKEY
051500970526     C                   END
051600970526     C     KEY           IFEQ      F10
051700970526     C                   MOVEL     '10'          FUNKEY
051800970526     C                   END
051900970526     C     KEY           IFEQ      F11
052000970526     C                   MOVEL     '11'          FUNKEY
052100970526     C                   END
052200970526     C     KEY           IFEQ      F12
052300970526     C                   MOVEL     '12'          FUNKEY
052400970526     C                   END
052500970526     C     KEY           IFEQ      F13
052600970526     C                   MOVEL     '13'          FUNKEY
052700970526     C                   END
052800970526     C     KEY           IFEQ      F14
052900970526     C                   MOVEL     '14'          FUNKEY
053000970526     C                   END
053100970526     C     KEY           IFEQ      F15
053200970526     C                   MOVEL     '15'          FUNKEY
053300970526     C                   END
053400970526     C     KEY           IFEQ      F16
053500970526     C                   MOVEL     '16'          FUNKEY
053600970526     C                   END
053700970526     C     KEY           IFEQ      F17
053800970526     C                   MOVEL     '17'          FUNKEY
053900970526     C                   END
054000970526     C     KEY           IFEQ      F18
054100970526     C                   MOVEL     '18'          FUNKEY
054200970526     C                   END
054300970526     C     KEY           IFEQ      F19
054400970526     C                   MOVEL     '19'          FUNKEY
054500970526     C                   END
054600970526     C     KEY           IFEQ      F20
054700970526     C                   MOVEL     '20'          FUNKEY
054800970526     C                   END
054900970526     C     KEY           IFEQ      F21
055000970526     C                   MOVEL     '21'          FUNKEY
055100970526     C                   END
055200970526     C     KEY           IFEQ      F22
055300970526     C                   MOVEL     '22'          FUNKEY
055400970526     C                   END
055500970526     C     KEY           IFEQ      F23
055600970526     C                   MOVEL     '23'          FUNKEY
055700970526     C                   END
055800970526     C     KEY           IFEQ      F24
055900970526     C                   MOVEL     '24'          FUNKEY
056000970526     C                   END
056100970526     C     KEY           IFEQ      ROLLUP
056200970526     C                   MOVEL     'RU'          FUNKEY
056300970526     C                   END
056400970526     C     KEY           IFEQ      HELP
056500970526     C                   MOVEL     'HP'          FUNKEY
056600970526     C                   END
056700970526     C                   ENDSR
056800970526     C*------------------------------------------------------------------------*
056900970526     C* NOTE COSTRUZIONE SCHIERE
057000970526     C*------------------------------------------------------------------------*
057100970526     C*
057200970526     C* -MSG-MESSAGGI
057300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
057400970526     C* (NON TOCCARE MSG,1 E MSG,2)
057500970526     C*
057600970526     C* -CMD-COMANDI
057700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
057800970526     C*
057900970526     C* -OPD-OPZIONI
058000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
058100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
058200970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
058300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
058400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
058500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
058600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
058700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
058800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
058900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
059000970526     C*
059100970526     C* -FUD-FUNZIONI
059200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
059300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
059400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
059500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
059600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
059700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
059800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
059900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
060000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
060100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
060200970526     O*------------------------------------------------------------------------*
060300970528** MSG - MESSAGGI
060400030916TIST71R: Tasto funzionale non gestito                                          1
060500051229TIST71R: Campo obbligatorio                                                    2
060600030626                                                                               3
060700030626                                                                               4
060800030626                                                                               5
060900030626                                                                               6
061000030626                                                                               7
061100030626                                                                               8
061200030626                                                                               9
061300030626                                                                               10
061400030626                                                                               11
061500030626                                                                               12
061600030626                                                                               13
061700030626                                                                               14
061800030626                                                                               15
061900030626                                                                               16
062000030626                                                                               17
062100030626                                                                               18
062200020415                                                                               19
062300020415                                                                               20
062400970526** FUD - FUNZIONI
062500970526                   -        (S/N)=N                        funz.01¬
062600970526                   -        (S/N)=N                        funz.02¬
062700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
062800970526                   -        (S/N)=N                        funz.04¬
062900970526                   -        (S/N)=N                        funz.05¬
063000000407F06=Conferma       -        (S/N)=S                        funz.06¬
063100970527                   -        (S/N)=N                        funz.07¬
063200970527                   -        (S/N)=N                        funz.08¬
063300970527                   -        (S/N)=N                        funz.09¬
063400970527                   -        (S/N)=N                        funz.10¬
063500970526                   -        (S/N)=N                        funz.11¬
063600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
063700970526                   -        (S/N)=N                        funz.13¬
063800970526                   -        (S/N)=N                        funz.14¬
063900970526                   -        (S/N)=N                        funz.15¬
064000970526                   -        (S/N)=N                        funz.16¬
064100970526                   -        (S/N)=N                        funz.17¬
064200970526                   -        (S/N)=N                        funz.18¬
064300970526                   -        (S/N)=N                        funz.19¬
064400970526                   -        (S/N)=N                        funz.20¬
064500970526                   -        (S/N)=N                        funz.21¬
064600970526                   -        (S/N)=N                        funz.22¬
064700970526                   -        (S/N)=N                        funz.23¬
064800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

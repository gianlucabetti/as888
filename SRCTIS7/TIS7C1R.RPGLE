000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800011206     Ftis7c1d   cf   e             WORKSTN INFDS(DEVDS1)
000900011206     F                                     SFILE(i7c1DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D* MESSAGGI
001400970526     D*------------
001500000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* DESCRIZIONI TASTI FUNZIONALI
001800970526     D*------------
001900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* FUNZIONI DA CARICARE A VIDEO
002200970526     D*------------
002300970526     D FUV             S             19    DIM(24)
002400970526     D*------------------------------------------------------------------------*
002500970526     D* INPUT
002600970526     D*------------------------------------------------------------------------*
002700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002800970526     D*------------
002900970526     D FUDDS           DS
003000970526     D  FUDDE1                 1      1
003100970526     D  FUDFUN                 2      3
003200970526     D  FUDDE2                 4     19
003300970526     D  FUDFI0                20     34
003400970526     D  FUDATT                35     35
003500970526     D  FUDFI1                36     80
003600970526     D  FUDDES                 1     19
003700970526     D  FUDRIG                 1     80
003800970526     D*------------
003900970526     D* DS GESTIONE TASTI FUNZIONALI
004000970526     D*------------
004100970526     D DSKEY         E DS
004200970526     D DEVDS1          DS
004300970526     D  KEY                  369    369
004400970526     D*------------
004500970526     D* DS "XMSG" - GESTIONE MESSAGGI
004600970526     D*------------
004700970526     D DSMSG         E DS
004800970526     D*------------
004900970526     D* ARCHITETTURA
005000970526     D*------------
005100970526     D KPJBA         E DS
005200991214     D*------------------
005300991214     D* DS X PASSAGGIO PARAMETRI
005400991214     D*------------------
005500991214     D PARAM           DS                  INZ
005600011206     D  PARORD                 1      1
005700970526     C*------------------------------------------------------------------------*
005800000303     C* MAIN LINES
005900970526     C*------------------------------------------------------------------------*
006000970526     C*
006100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006200991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
006300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
006400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
006500970526     C*
006600970526     C* EMETTE FORMATO VIDEO
006700970526     C                   EXSR      BUTFMR
006800970526     C*
006900970526     C* GESTIONE TASTI FUNZIONALI
007000970526     C                   EXSR      FUNCON
007100970526E   2C                   ENDDO
007200970526     C*
007300970526     C* FINE PGM
007400970526     C                   SETON                                        LR
007500970526     C*------------------------------------------------------------------------*
007600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007700970526     C*------------------------------------------------------------------------*
007800970526     C     BUTFMR        BEGSR
007900970526     C*
008000970526     C* EMETTE LA TESTATA
008100011206     C                   WRITE     i7c1D01
008200970526     C*
008300970526     C* SPEDISCE EVENTUALE MESSAGGIO
008400970526     C                   EXSR      SNDMSG
008500970526     C*
008600970526     C* EMETTE LE FUNZIONI
008700011206     C                   WRITE     i7c1D02
008800970526     C*
008900970526     C* EMETTE IL VIDEO
009000011206     C                   EXFMT     i7c1D03
009100970526     C*
009200991214     C* REIMPOSTA FLAG DI ERRORE
009300991214     C                   MOVEL     '0'           ERRL00
009400970526     C*
009500970526     C* REIMPOSTA INDICATORI DI ERRORE
009600991214     C*
009700011205     C                   SETOFF                                       535459
009800991214     C*
009900970526     C*
010000970526     C                   ENDSR
010100970526     C*------------------------------------------------------------------------*
010200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010300970526     C*------------------------------------------------------------------------*
010400970526     C     SNDMSG        BEGSR
010500970526     C*
010600970526     C* PULISCE IL SFILE MESSAGGI
010700970526     C                   MOVEL     '1'           *IN40
010800011206     C                   WRITE     i7c1DMC
010900970526     C                   MOVEL     '0'           *IN40
011000970526     C*
011100970526     C* VISUALIZZA IL MESSAGGIO
011200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
011300970526     C                   CALL      'XMSG'        XMSGPA
011400970526IF  1C     DSMSER        IFEQ      *ZEROS
011500970526     C                   MOVEL     DSMSKE        MSGKEY
011600970526E   1C                   ENDIF
011700970526     C                   Z-ADD     1             MSGULT
011800011206     C                   WRITE     i7c1DMS
011900011206     C                   WRITE     i7c1DMC
012000970526     C*
012100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
012200970526     C                   MOVEL     *BLANKS       DSMSMS
012300970526     C*
012400970526     C                   ENDSR
012500970526     C*------------------------------------------------------------------------*
012600970526     C* ENTGES - GESTIONE ENTER -> INVIO
012700970526     C*------------------------------------------------------------------------*
012800970526     C     ENTGES        BEGSR
012900970526     C*
013000970526     C* CONTROLLA I CAMPI IMMESSI
013100970526     C                   EXSR      DECODI
013200970526     C*
013300970526     C                   ENDSR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* F03GES - GESTIONE F03 -> FINE
013600970526     C*------------------------------------------------------------------------*
013700970526     C     F03GES        BEGSR
013800970526     C*
013900970526     C* IMPOSTA "PREMUTO F03"
014000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400991214     C* F06GES - GESTIONE F06 -> CONFERMA
014500970526     C*------------------------------------------------------------------------*
014600970526     C     F06GES        BEGSR
014700970526     C*
014800991214     C* CONTROLLA I CAMPI IMMESSI
014900991214     C                   EXSR      DECODI
015000991214     C     ERRL00        IFEQ      '0'
015100991214     C* ESEGUE ROUTINE DI CONFERMA
015200991214     C                   EXSR      ESEGUI
015201020315     C                   MOVEL     '1'           FINL00
015300991214     C                   ENDIF
015400970526     C*
015500991214     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F12GES - GESTIONE F12 -> RITORNO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F12GES        BEGSR
016000970526     C*
016100970526     C* IMPOSTA "PREMUTO F12"
016200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
016700970526     C*------------------------------------------------------------------------*
016800970526     C     F24GES        BEGSR
016900970526     C*
017000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
017100970526     C*
017200970526     C                   ENDSR
017300991214     C*------------------------------------------------------------------------*
017400991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
017500991214     C*------------------------------------------------------------------------*
017600991214     C     PASPAR        BEGSR
017700991214     C*
017800011206     C                   MOVEL     VIDORD        PARORD
017900991214     C*
018000991214     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200011205     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
018300970526     C*------------------------------------------------------------------------*
018400991214     C     ESEGUI        BEGSR
018500991214     C*
018600991214     C*
018700991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
018800011206     C                   MOVEL     'S7C2'        KCOAZ
018900991214     C                   EXSR      PASPAR
019000991214     C                   MOVEL     PARAM         KPJBU
019100020207     C                   CALL      'BCH10'
019200020207 MOM C*                  CALL      'TIS7C2R'
019300991214     C                   PARM                    KPJBA
019400991214     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019800970526     C*------------------------------------------------------------------------*
019900970526     C     DECODI        BEGSR
020000000306     C*
020100000306     C*
020200970528     C                   ENDSR
020300970526     C*------------------------------------------------------------------------*
020400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
020500970526     C*------------------------------------------------------------------------*
020600970526     C     AZZVID        BEGSR
020700011206     C*
020800970526     C* CAMPI
020900011206     C                   MOVEL     *blanks       VIDORD
021000970526     C*
021100970526     C                   ENDSR
021200000303     C*------------------------------------------------------------------------*
021300000905     C* IMPVID - IMPOSTA I DEFAULT A VIDEO
021400000303     C*------------------------------------------------------------------------*
021500000303     C     IMPVID        BEGSR
021600000303     C*
021700011206     C                   MOVEL     'D'           VIDORD
021800000303     C*
021900000303     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
022200970526     C*------------------------------------------------------------------------*
022300970526     C     IMPFUN        BEGSR
022400970526     C*
022500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
022600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
022700970526     C                   EXSR      FUNGES
022800970526     C*
022900970526     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* *INZSR - OPERAZIONI INIZIALI
023200970526     C*------------------------------------------------------------------------*
023300970526     C     *INZSR        BEGSR
023400991217     C*
023500991217     C     *ENTRY        PLIST
023600991217     C                   PARM                    KPJBA
023700970526     C*
023800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
023900970526     C     XMSGPA        PLIST
024000970526     C                   PARM                    DSMSMS
024100970526     C                   PARM                    DSMSPG
024200970526     C                   PARM                    DSMSKE
024300970526     C                   PARM                    DSMSER
024400970526     C*
024500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
024600970526     C                   CALL      'XKEY'
024700970526     C                   PARM                    DSKEY
024800970526     C*---
024900970526     C* VARIABILI NON RIFERITE AL DATA BASE
025000970526     C*---
025100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
025200991214     C                   MOVEL     *BLANKS       DEPOP1            3
025300970526     C*---
025400970526     C* VARIABILI DI CONTROLLO
025500970526     C*---
025600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
025700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
025800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
025900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
026000970526     C*
026100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
026200011206     C                   MOVEL     'TIS7C1R'     DSMSPG
026300011206     C                   MOVEL     'TIS7C1R'     MSGQUE
026400970526     C                   MOVEL     *BLANKS       MSGKEY
026500970526     C                   MOVEL     *BLANKS       DSMSMS
026600000310     C*
026700000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
026800000310     C                   EXSR      AZZVID
026900000310     C                   EXSR      IMPVID
027000970526     C*
027100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
027200970526     C                   EXSR      IMPFUN
027300970526     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
027700970526     C*------------------------------------------------------------------------*
027800970526     C     FUNGES        BEGSR
027900970526     C*
028000970526     C* PULISCO CAMPI DI LAVORO
028100970526     C                   Z-ADD     0             W                 5 0
028200970526     C*
028300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
028400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
028500970526     C     1             DO        24            I
028600970526     C                   MOVEL     FUD(I)        FUDRIG
028700970526     C*
028800970526     C* PULIZIA FUNZIONI PRECEDENTI
028900970526     C                   MOVEL     *BLANKS       FUV(I)
029000970526     C*
029100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
029200970526     C* (SALTA F24)
029300970526     C     FUDATT        IFEQ      'S'
029400970526     C     FUDFUN        ANDNE     '24'
029500970526     C                   ADD       1             W
029600970526     C                   MOVEL     FUDDES        FUV(W)
029700970526     C                   END
029800970526     C                   END                                                    DO WHILE
029900970526     C*
030000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
030100970526     C* ALTRIMENTI DISATTIVO F24
030200970526     C     1             DO        24            I
030300970526     C                   MOVEL     FUD(I)        FUDRIG
030400970526     C*
030500970526     C     FUDFUN        IFEQ      '24'
030600970526     C     W             IFGT      6
030700970526     C                   MOVEL     'S'           FUDATT
030800970526     C                   ELSE
030900970526     C                   MOVEL     *BLANKS       FUDATT
031000970526     C                   END                                                    W GT 8
031100970526     C                   MOVEL     FUDRIG        FUD(I)
031200970526     C                   END
031300970526     C                   END                                                    DO WHILE
031400970526     C*
031500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
031600970526     C     W             DIV       6             I
031700970526     C                   MVR                     Z
031800970526     C*
031900970526     C     Z             IFNE      0
032000970526     C     I             ADD       1             Z
032100970526     C                   ELSE
032200970526     C                   Z-ADD     I             Z                 5 0
032300970526     C                   END
032400970526     C*
032500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
032600970526     C                   ADD       1             FUNGRU
032700970526     C     FUNGRU        IFGT      Z
032800970526     C     FUNGRU        ORLT      1
032900970526     C                   Z-ADD     1             FUNGRU
033000970526     C                   END
033100970526     C*
033200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
033300970526     C     FUNGRU        IFEQ      1
033400970526     C                   MOVEL     FUV(1)        FUN001
033500970526     C                   MOVEL     FUV(2)        FUN002
033600970526     C                   MOVEL     FUV(3)        FUN003
033700970526     C                   MOVEL     FUV(4)        FUN004
033800970526     C                   MOVEL     FUV(5)        FUN005
033900970526     C                   MOVEL     FUV(6)        FUN006
034000970526     C                   END
034100970526     C*
034200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
034300970526     C     FUNGRU        IFEQ      2
034400970526     C                   MOVEL     FUV(7)        FUN001
034500970526     C                   MOVEL     FUV(8)        FUN002
034600970526     C                   MOVEL     FUV(9)        FUN003
034700970526     C                   MOVEL     FUV(10)       FUN004
034800970526     C                   MOVEL     FUV(11)       FUN005
034900970526     C                   MOVEL     FUV(12)       FUN006
035000970526     C                   END
035100970526     C*
035200970526     C* TERZO GRUPPO DI 6 FUNZIONI
035300970526     C     FUNGRU        IFEQ      3
035400970526     C                   MOVEL     FUV(13)       FUN001
035500970526     C                   MOVEL     FUV(14)       FUN002
035600970526     C                   MOVEL     FUV(15)       FUN003
035700970526     C                   MOVEL     FUV(16)       FUN004
035800970526     C                   MOVEL     FUV(17)       FUN005
035900970526     C                   MOVEL     FUV(18)       FUN006
036000970526     C                   END
036100970526     C*
036200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
036300970526     C     FUNGRU        IFEQ      4
036400970526     C                   MOVEL     FUV(19)       FUN001
036500970526     C                   MOVEL     FUV(20)       FUN002
036600970526     C                   MOVEL     FUV(21)       FUN003
036700970526     C                   MOVEL     FUV(22)       FUN004
036800970526     C                   MOVEL     *BLANKS       FUN005
036900970526     C                   MOVEL     *BLANKS       FUN006
037000970526     C                   END
037100970526     C*
037200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
037300970526     C                   MOVEL     *BLANKS       FUN007
037400970526     C                   MOVEL     *BLANKS       FUN008
037500970526     C*
037600970526     C     1             DO        24            I
037700970526     C                   MOVEL     FUD(I)        FUDRIG
037800970526     C     FUDFUN        IFEQ      '24'
037900970526     C     FUDATT        ANDEQ     'S'
038000970526     C                   MOVEL     FUDDES        FUN008
038100970526     C                   END
038200970526     C                   END                                                    DO
038300970526     C*
038400970526     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
038700970526     C*------------------------------------------------------------------------*
038800970526     C     FUNCON        BEGSR
038900970526     C*
039000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
039100970526     C                   EXSR      FUNVAR
039200970526     C*
039300970526     C* PULISCO CAMPI DI LAVORO
039400970526     C                   Z-ADD     0             I                 5 0
039500970526     C                   Z-ADD     0             W                 5 0
039600970526     C*
039700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
039800970526     C     FUNKEY        IFNE      *BLANKS
039900970526     C     1             DO        24            I
040000970526     C                   MOVEL     FUD(I)        FUDRIG
040100970526     C*
040200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
040300970526     C     FUNKEY        IFEQ      FUDFUN
040400970526     C     FUDATT        ANDEQ     'S'
040500970526     C                   Z-ADD     I             W
040600970526     C                   END
040700970526     C                   END                                                    DO WHILE
040800970526     C*
040900970526     C* FUNZIONE NON ATTIVA (ERRORE)
041000970526     C     W             IFEQ      0
041100970526     C*
041200970526     C* EMISSIONE MESSAGGIO DI ERRORE
041300970526     C                   MOVEL     MSG(1)        DSMSMS
041400970526     C*
041500970526     C                   END                                                    W=0
041600970526     C                   END                                                    FUNKEY NE *BLANKS
041700970526     C*
041800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
041900970526     C     W             IFNE      0
042000970526     C     FUNKEY        OREQ      *BLANKS
042100970526     C     FUNKEY        IFEQ      *BLANKS
042200970526     C                   EXSR      ENTGES
042300970526     C                   END
042400970526     C     FUNKEY        IFEQ      '03'
042500970526     C                   EXSR      F03GES
042600970526     C                   END
042700970526     C     FUNKEY        IFEQ      '06'
042800970526     C                   EXSR      F06GES
042900970526     C                   END
043000970526     C     FUNKEY        IFEQ      '12'
043100970526     C                   EXSR      F12GES
043200970526     C                   END
043300970526     C     FUNKEY        IFEQ      '24'
043400970526     C                   EXSR      F24GES
043500970526     C                   END
043600970526     C                   END
043700970526     C*
043800970526     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
044100970526     C*------------------------------------------------------------------------*
044200970526     C     FUNVAR        BEGSR
044300970526     C*
044400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
044500970526     C*
044600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
044700970526     C     KEY           IFEQ      ENTER
044800970526     C                   MOVEL     *BLANKS       FUNKEY
044900970526     C                   END
045000970526     C     KEY           IFEQ      F1
045100970526     C                   MOVEL     '01'          FUNKEY
045200970526     C                   END
045300970526     C     KEY           IFEQ      F2
045400970526     C                   MOVEL     '02'          FUNKEY
045500970526     C                   END
045600970526     C     KEY           IFEQ      F3
045700970526     C                   MOVEL     '03'          FUNKEY
045800970526     C                   END
045900970526     C     KEY           IFEQ      F4
046000970526     C                   MOVEL     '04'          FUNKEY
046100970526     C                   END
046200970526     C     KEY           IFEQ      F5
046300970526     C                   MOVEL     '05'          FUNKEY
046400970526     C                   END
046500970526     C     KEY           IFEQ      F6
046600970526     C                   MOVEL     '06'          FUNKEY
046700970526     C                   END
046800970526     C     KEY           IFEQ      F7
046900970526     C                   MOVEL     '07'          FUNKEY
047000970526     C                   END
047100970526     C     KEY           IFEQ      F8
047200970526     C                   MOVEL     '08'          FUNKEY
047300970526     C                   END
047400970526     C     KEY           IFEQ      F9
047500970526     C                   MOVEL     '09'          FUNKEY
047600970526     C                   END
047700970526     C     KEY           IFEQ      F10
047800970526     C                   MOVEL     '10'          FUNKEY
047900970526     C                   END
048000970526     C     KEY           IFEQ      F11
048100970526     C                   MOVEL     '11'          FUNKEY
048200970526     C                   END
048300970526     C     KEY           IFEQ      F12
048400970526     C                   MOVEL     '12'          FUNKEY
048500970526     C                   END
048600970526     C     KEY           IFEQ      F13
048700970526     C                   MOVEL     '13'          FUNKEY
048800970526     C                   END
048900970526     C     KEY           IFEQ      F14
049000970526     C                   MOVEL     '14'          FUNKEY
049100970526     C                   END
049200970526     C     KEY           IFEQ      F15
049300970526     C                   MOVEL     '15'          FUNKEY
049400970526     C                   END
049500970526     C     KEY           IFEQ      F16
049600970526     C                   MOVEL     '16'          FUNKEY
049700970526     C                   END
049800970526     C     KEY           IFEQ      F17
049900970526     C                   MOVEL     '17'          FUNKEY
050000970526     C                   END
050100970526     C     KEY           IFEQ      F18
050200970526     C                   MOVEL     '18'          FUNKEY
050300970526     C                   END
050400970526     C     KEY           IFEQ      F19
050500970526     C                   MOVEL     '19'          FUNKEY
050600970526     C                   END
050700970526     C     KEY           IFEQ      F20
050800970526     C                   MOVEL     '20'          FUNKEY
050900970526     C                   END
051000970526     C     KEY           IFEQ      F21
051100970526     C                   MOVEL     '21'          FUNKEY
051200970526     C                   END
051300970526     C     KEY           IFEQ      F22
051400970526     C                   MOVEL     '22'          FUNKEY
051500970526     C                   END
051600970526     C     KEY           IFEQ      F23
051700970526     C                   MOVEL     '23'          FUNKEY
051800970526     C                   END
051900970526     C     KEY           IFEQ      F24
052000970526     C                   MOVEL     '24'          FUNKEY
052100970526     C                   END
052200970526     C     KEY           IFEQ      ROLLUP
052300970526     C                   MOVEL     'RU'          FUNKEY
052400970526     C                   END
052500970526     C     KEY           IFEQ      HELP
052600970526     C                   MOVEL     'HP'          FUNKEY
052700970526     C                   END
052800970526     C                   ENDSR
052900970526     C*------------------------------------------------------------------------*
053000970526     C* NOTE COSTRUZIONE SCHIERE
053100970526     C*------------------------------------------------------------------------*
053200970526     C*
053300970526     C* -MSG-MESSAGGI
053400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
053500970526     C* (NON TOCCARE MSG,1 E MSG,2)
053600970526     C*
053700970526     C* -CMD-COMANDI
053800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
053900970526     C*
054000970526     C* -OPD-OPZIONI
054100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
054200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
054300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
054400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
054500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
054600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
054700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
054800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
054900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
055000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
055100970526     C*
055200970526     C* -FUD-FUNZIONI
055300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
055400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
055500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
055600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
055700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
055800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
055900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
056000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
056100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
056200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
056300970526     O*------------------------------------------------------------------------*
056400970526** MSG - MESSAGGI
056500011206TIS7C1R- Tasto funzionale non gestito                                          1
056600011206TIS7C1R-                                                                       2
056700011206TIS7C1R-                                                                       3
056800011206TIS7C1R-                                                                       4
056900011206TIS7C1R-                                                                       5
057000011206TIS7C1R-                                                                       6
057100011206TIS7C1R- Cliente incompatibile per Filiale selezionata.                        7
057200011206TIS7C1R- Cliente non autorizzato.                                              8
057300011206TIS7C1R- Punto operativo non autorizzato.                                      9
057400011206TIS7C1R- Anno inserito non accettabile.                                        10
057500011206TIS7C1R- Mese inserito non valido.                                             11
057600011206TIS7C1R- Tipo legame errato verificarne sulla tabella l'esistenza              12
057700011206TIS7C1R- Punto operativo errato.                                               13
057800011206TIS7C1R- Data errata                                                           14
057900011206TIS7C1R- La data scadenza deve essere maggiore della decorrenza                15
058000011206TIS7C1R- Cliente inserito inesistente.                                         16
058100011206TIS7C1R- Cliente inserito sospeso.                                             17
058200011206TIS7C1R- Tipo lancio obbligatorio.                                             18
058300011206TIS7C1R- Cliente inserito è senza P.IVA o Cod.Fisc.                            19
058400011206TIS7C1R- Il codice figlio non può essere uguale al codice padre.               20
058500011206TIS7C1R- Scelta obbligatoria.                                                  21
058600011206TIS7C1R- Tipo legame non autorizzato.                                          22
058700011206TIS7C1R- Filiale non abilitata alla gestione dei clienti scelti.               23
058800011206TIS7C1R-                                                                       24
058900970526** FUD - FUNZIONI
059000970526                   -        (S/N)=N                        funz.01¬
059100970526                   -        (S/N)=N                        funz.02¬
059200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
059300970526                   -        (S/N)=N                        funz.04¬
059400970526                   -        (S/N)=N                        funz.05¬
059500991214F06=Conferma       -        (S/N)=S                        funz.06¬
059600970527                   -        (S/N)=N                        funz.07¬
059700970527                   -        (S/N)=N                        funz.08¬
059800970527                   -        (S/N)=N                        funz.09¬
059900970527                   -        (S/N)=N                        funz.10¬
060000970526                   -        (S/N)=N                        funz.11¬
060100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
060200970526                   -        (S/N)=N                        funz.13¬
060300970526                   -        (S/N)=N                        funz.14¬
060400970526                   -        (S/N)=N                        funz.15¬
060500970526                   -        (S/N)=N                        funz.16¬
060600970526                   -        (S/N)=N                        funz.17¬
060700970526                   -        (S/N)=N                        funz.18¬
060800970526                   -        (S/N)=N                        funz.19¬
060900970526                   -        (S/N)=N                        funz.20¬
061000970526                   -        (S/N)=N                        funz.21¬
061100970526                   -        (S/N)=N                        funz.22¬
061200970526                   -        (S/N)=N                        funz.23¬
061300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

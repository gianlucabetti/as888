000100970526     H*------------------------------------------------------------------------*
000200991008     H* Gestione codici unificanti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020211     Ftis7c5d   cf   e             WORKSTN INFDS(DEVDS1)
000900020211     F                                     SFILE(s7c5DMS:MSGULT)
001000000306     Fazorg01l  IF   E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D* MESSAGGI
001500970526     D*------------
001600000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
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
002800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002900970526     D*------------
003000970526     D FUDDS           DS
003100970526     D  FUDDE1                 1      1
003200970526     D  FUDFUN                 2      3
003300970526     D  FUDDE2                 4     19
003400970526     D  FUDFI0                20     34
003500970526     D  FUDATT                35     35
003600970526     D  FUDFI1                36     80
003700970526     D  FUDDES                 1     19
003800970526     D  FUDRIG                 1     80
003900970526     D*------------
004000970526     D* DS GESTIONE TASTI FUNZIONALI
004100970526     D*------------
004200970526     D DSKEY         E DS
004300970526     D DEVDS1          DS
004400970526     D  KEY                  369    369
004500970526     D*------------
004600970526     D* DS "XMSG" - GESTIONE MESSAGGI
004700970526     D*------------
004800970526     D DSMSG         E DS
004900970526     D*------------
005000970526     D* ARCHITETTURA
005100970526     D*------------
005200970526     D KPJBA         E DS
005300020211     D*------------------
005400020211     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500020211     D*------------------
005600020211     D WLBDA8          DS                  INZ
005700020211     D  G08DAT                 1      8  0
005800020211     D  G08INV                 9     16  0
005900020211     D  G08ERR                17     17
006000020211     D  G08TGI                18     22  0
006100020710     D*------------------
006200020710     D* DS X CONTROLLO/RICHIESTA TABELLA
006300020710     D*------------
006400020710     D TIBS02DS      E DS
006500991214     D*------------------
006600991214     D* DS X PASSAGGIO PARAMETRI
006700991214     D*------------------
006800991214     D PARAM           DS                  INZ
006900020211     D  PARATT                 1      1
007000020211     D  PARDAT                 2      9  0
007100020211     D  PARDAI                10     17  0
007200020710     D  PARTLE                18     19
007300970526     C*------------------------------------------------------------------------*
007400000303     C* MAIN LINES
007500970526     C*------------------------------------------------------------------------*
007600970526     C*
007700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008100970526     C*
008200970526     C* EMETTE FORMATO VIDEO
008300970526     C                   EXSR      BUTFMR
008400970526     C*
008500970526     C* GESTIONE TASTI FUNZIONALI
008600970526     C                   EXSR      FUNCON
008700970526E   2C                   ENDDO
008800970526     C*
008900970526     C* FINE PGM
009000970526     C                   SETON                                        LR
009100970526     C*------------------------------------------------------------------------*
009200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009300970526     C*------------------------------------------------------------------------*
009400970526     C     BUTFMR        BEGSR
009500970526     C*
009600970526     C* EMETTE LA TESTATA
009700020211     C                   WRITE     s7c5D01
009800970526     C*
009900970526     C* SPEDISCE EVENTUALE MESSAGGIO
010000970526     C                   EXSR      SNDMSG
010100970526     C*
010200970526     C* EMETTE LE FUNZIONI
010300020211     C                   WRITE     s7c5D02
010400970526     C*
010500970526     C* EMETTE IL VIDEO
010600020211     C                   EXFMT     s7c5D03
010700970526     C*
010800991214     C* REIMPOSTA FLAG DI ERRORE
010900991214     C                   MOVEL     '0'           ERRL00
011000970526     C*
011100970526     C* REIMPOSTA INDICATORI DI ERRORE
011200991214     C*
011300011205     C                   SETOFF                                       535459
011400970526     C*
011500970526     C                   ENDSR
011600970526     C*------------------------------------------------------------------------*
011700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011800970526     C*------------------------------------------------------------------------*
011900970526     C     SNDMSG        BEGSR
012000970526     C*
012100970526     C* PULISCE IL SFILE MESSAGGI
012200970526     C                   MOVEL     '1'           *IN40
012300020211     C                   WRITE     s7c5DMC
012400970526     C                   MOVEL     '0'           *IN40
012500970526     C*
012600970526     C* VISUALIZZA IL MESSAGGIO
012700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012800970526     C                   CALL      'XMSG'        XMSGPA
012900970526IF  1C     DSMSER        IFEQ      *ZEROS
013000970526     C                   MOVEL     DSMSKE        MSGKEY
013100970526E   1C                   ENDIF
013200970526     C                   Z-ADD     1             MSGULT
013300020211     C                   WRITE     s7c5DMS
013400020211     C                   WRITE     s7c5DMC
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
014500970526     C* CONTROLLA I CAMPI IMMESSI
014600970526     C                   EXSR      DECODI
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
015900991214     C* F06GES - GESTIONE F06 -> CONFERMA
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F06GES        BEGSR
016200970526     C*
016300991214     C* CONTROLLA I CAMPI IMMESSI
016400991214     C                   EXSR      DECODI
016500991214     C     ERRL00        IFEQ      '0'
016600991214     C* ESEGUE ROUTINE DI CONFERMA
016700991214     C                   EXSR      ESEGUI
016800020710     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016900991214     C                   ENDIF
017000970526     C*
017100991214     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F12GES - GESTIONE F12 -> RITORNO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F12GES        BEGSR
017600970526     C*
017700970526     C* IMPOSTA "PREMUTO F12"
017800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F24GES        BEGSR
018500970526     C*
018600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
018700970526     C*
018800970526     C                   ENDSR
018900991214     C*------------------------------------------------------------------------*
019000991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
019100991214     C*------------------------------------------------------------------------*
019200991214     C     PASPAR        BEGSR
019300991214     C*
019400020211     C                   MOVEL     VIDATT        PARATT
019500020211     C                   Z-ADD     DATCOR        PARDAT
019600020211     C                   Z-ADD     WN8           PARDAI
019700020710     C                   MOVEL     VIDTLE        PARTLE
019800991214     C*
019900991214     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100011205     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
020200970526     C*------------------------------------------------------------------------*
020300991214     C     ESEGUI        BEGSR
020400991214     C*
020500991214     C*
020600991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
020700020211     C                   MOVEL     'S7C6'        KCOAZ
020800991214     C                   EXSR      PASPAR
020900991214     C                   MOVEL     PARAM         KPJBU
021000020710     C                   CALL      'BCH10'
021100020710 MOM C*                  CALL      'TIS7C6R'
021200991214     C                   PARM                    KPJBA
021300991214     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
021700970526     C*------------------------------------------------------------------------*
021800970526     C     DECODI        BEGSR
021900000306     C*
022000020710     C* CONTROLLA IL TIPO LEGAME
022100020710     C                   EXSR      CNTTLE
022200000306     C*
022300000306     C                   ENDSR
022400020710     C*------------------------------------------------------------------------*
022500020710     C* CNTTLE - CONTROLLA TIPO LEGAME
022600020710     C*------------------------------------------------------------------------*
022700020710     C     CNTTLE        BEGSR
022800020710     C*
022900020710     C     *LIKE         DEFINE    VIDTLE        WvidTLE
023000020710     C                   MOVEL     VIDTLE        WvidTLE
023100020710     C*
023200020710     C* VERIFICO CHE NN SIANO STATI RICHIESTI TUTTI I LEGAMI
023300020710     C     '*'           SCAN      WvidTLE                                31
023400020710     C                   IF        *IN31 = *OFF
023500020710     C*
023600020710     C*  RICERCA
023700020710     C                   CLEAR                   TIBS02DS
023800020710     C     '?'           SCAN      WvidTLE                                30
023900020710     C                   EVAL      t02tla = 'L'
024000020710     C                   IF        *IN30 = *ON
024100020710     C                   MOVEL     'R'           t02mod
024200020710     C                   ELSE
024300020710     C                   MOVEL     'C'           t02mod
024400020710     C                   MOVEL(p)  WvidTLE       t02ke1
024500020710     C                   ENDIF
024600020710     C                   MOVEL     'TLE'         t02cod
024700020710     C                   MOVEL     KNSIF         t02sif
024800020710     C                   CALL      'TIBS02R'
024900020710     C                   PARM                    KPJBA
025000020710     C                   PARM                    TIBS02DS
025100020710     C
025200020710     C*
025300020710     C                   IF        t02err = *blanks
025400020710     C                   MOVEL     t02ke1        VIDTLE
025500020710     C                   ELSE
025600020710     C                   MOVEL     '1'           ERRL00
025700020710     C                   SETON                                        86
025800020710     C                   MOVEL     MSG(2)        DSMSMS
025900020710     C                   ENDIF
026000020710     C                   ENDIF
026100020710     C*
026200020710     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
026500970526     C*------------------------------------------------------------------------*
026600970526     C     AZZVID        BEGSR
026700020211     C*
026800020211     C                   MOVEL     *blanks       VIDATT
026900020710     C                   MOVEL     *blanks       VIDTLE
027000970526     C*
027100970526     C                   ENDSR
027200000303     C*------------------------------------------------------------------------*
027300000905     C* IMPVID - IMPOSTA I DEFAULT A VIDEO
027400000303     C*------------------------------------------------------------------------*
027500000303     C     IMPVID        BEGSR
027600000303     C*
027700020211     C                   MOVEL     'S'           VIDATT
027800020710     C                   MOVEL     'WW'          VIDTLE
027900000303     C*
028000000303     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     IMPFUN        BEGSR
028500970526     C*
028600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
028700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
028800970526     C                   EXSR      FUNGES
028900970526     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* *INZSR - OPERAZIONI INIZIALI
029300970526     C*------------------------------------------------------------------------*
029400970526     C     *INZSR        BEGSR
029500991217     C*
029600991217     C     *ENTRY        PLIST
029700991217     C                   PARM                    KPJBA
029800970526     C*
029900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
030000970526     C     XMSGPA        PLIST
030100970526     C                   PARM                    DSMSMS
030200970526     C                   PARM                    DSMSPG
030300970526     C                   PARM                    DSMSKE
030400970526     C                   PARM                    DSMSER
030500970526     C*
030600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
030700970526     C                   CALL      'XKEY'
030800970526     C                   PARM                    DSKEY
030900970526     C*---
031000970526     C* VARIABILI NON RIFERITE AL DATA BASE
031100970526     C*---
031200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
031300991214     C                   MOVEL     *BLANKS       DEPOP1            3
031400970526     C*---
031500970526     C* VARIABILI DI CONTROLLO
031600970526     C*---
031700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
031800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
031900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
032000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
032100970526     C*
032200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
032300020211     C                   MOVEL     'TIS7C5R'     DSMSPG
032400020211     C                   MOVEL     'TIS7C5R'     MSGQUE
032500970526     C                   MOVEL     *BLANKS       MSGKEY
032600970526     C                   MOVEL     *BLANKS       DSMSMS
032700970528     C*
032800970528     C* CALCOLA LA DATA CORRENTE
032900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
033000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
033100020211     C                   Z-ADD     WN8           G08DAT
033200020211     C                   Z-ADD     *ZEROS        G08INV
033300020211     C                   MOVEL     '0'           G08ERR
033400020211     C                   CALL      'XSRDA8'
033500020211     C                   PARM                    WLBDA8
033600020211     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
033700000310     C*
033800000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
033900000310     C                   EXSR      AZZVID
034000000310     C                   EXSR      IMPVID
034100970526     C*
034200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
034300970526     C                   EXSR      IMPFUN
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
034800970526     C*------------------------------------------------------------------------*
034900970526     C     FUNGES        BEGSR
035000970526     C*
035100970526     C* PULISCO CAMPI DI LAVORO
035200970526     C                   Z-ADD     0             W                 5 0
035300970526     C*
035400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
035500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
035600970526     C     1             DO        24            I
035700970526     C                   MOVEL     FUD(I)        FUDRIG
035800970526     C*
035900970526     C* PULIZIA FUNZIONI PRECEDENTI
036000970526     C                   MOVEL     *BLANKS       FUV(I)
036100970526     C*
036200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
036300970526     C* (SALTA F24)
036400970526     C     FUDATT        IFEQ      'S'
036500970526     C     FUDFUN        ANDNE     '24'
036600970526     C                   ADD       1             W
036700970526     C                   MOVEL     FUDDES        FUV(W)
036800970526     C                   END
036900970526     C                   END                                                    DO WHILE
037000970526     C*
037100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
037200970526     C* ALTRIMENTI DISATTIVO F24
037300970526     C     1             DO        24            I
037400970526     C                   MOVEL     FUD(I)        FUDRIG
037500970526     C*
037600970526     C     FUDFUN        IFEQ      '24'
037700970526     C     W             IFGT      6
037800970526     C                   MOVEL     'S'           FUDATT
037900970526     C                   ELSE
038000970526     C                   MOVEL     *BLANKS       FUDATT
038100970526     C                   END                                                    W GT 8
038200970526     C                   MOVEL     FUDRIG        FUD(I)
038300970526     C                   END
038400970526     C                   END                                                    DO WHILE
038500970526     C*
038600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
038700970526     C     W             DIV       6             I
038800970526     C                   MVR                     Z
038900970526     C*
039000970526     C     Z             IFNE      0
039100970526     C     I             ADD       1             Z
039200970526     C                   ELSE
039300970526     C                   Z-ADD     I             Z                 5 0
039400970526     C                   END
039500970526     C*
039600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
039700970526     C                   ADD       1             FUNGRU
039800970526     C     FUNGRU        IFGT      Z
039900970526     C     FUNGRU        ORLT      1
040000970526     C                   Z-ADD     1             FUNGRU
040100970526     C                   END
040200970526     C*
040300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
040400970526     C     FUNGRU        IFEQ      1
040500970526     C                   MOVEL     FUV(1)        FUN001
040600970526     C                   MOVEL     FUV(2)        FUN002
040700970526     C                   MOVEL     FUV(3)        FUN003
040800970526     C                   MOVEL     FUV(4)        FUN004
040900970526     C                   MOVEL     FUV(5)        FUN005
041000970526     C                   MOVEL     FUV(6)        FUN006
041100970526     C                   END
041200970526     C*
041300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
041400970526     C     FUNGRU        IFEQ      2
041500970526     C                   MOVEL     FUV(7)        FUN001
041600970526     C                   MOVEL     FUV(8)        FUN002
041700970526     C                   MOVEL     FUV(9)        FUN003
041800970526     C                   MOVEL     FUV(10)       FUN004
041900970526     C                   MOVEL     FUV(11)       FUN005
042000970526     C                   MOVEL     FUV(12)       FUN006
042100970526     C                   END
042200970526     C*
042300970526     C* TERZO GRUPPO DI 6 FUNZIONI
042400970526     C     FUNGRU        IFEQ      3
042500970526     C                   MOVEL     FUV(13)       FUN001
042600970526     C                   MOVEL     FUV(14)       FUN002
042700970526     C                   MOVEL     FUV(15)       FUN003
042800970526     C                   MOVEL     FUV(16)       FUN004
042900970526     C                   MOVEL     FUV(17)       FUN005
043000970526     C                   MOVEL     FUV(18)       FUN006
043100970526     C                   END
043200970526     C*
043300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
043400970526     C     FUNGRU        IFEQ      4
043500970526     C                   MOVEL     FUV(19)       FUN001
043600970526     C                   MOVEL     FUV(20)       FUN002
043700970526     C                   MOVEL     FUV(21)       FUN003
043800970526     C                   MOVEL     FUV(22)       FUN004
043900970526     C                   MOVEL     *BLANKS       FUN005
044000970526     C                   MOVEL     *BLANKS       FUN006
044100970526     C                   END
044200970526     C*
044300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
044400970526     C                   MOVEL     *BLANKS       FUN007
044500970526     C                   MOVEL     *BLANKS       FUN008
044600970526     C*
044700970526     C     1             DO        24            I
044800970526     C                   MOVEL     FUD(I)        FUDRIG
044900970526     C     FUDFUN        IFEQ      '24'
045000970526     C     FUDATT        ANDEQ     'S'
045100970526     C                   MOVEL     FUDDES        FUN008
045200970526     C                   END
045300970526     C                   END                                                    DO
045400970526     C*
045500970526     C                   ENDSR
045600970526     C*------------------------------------------------------------------------*
045700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
045800970526     C*------------------------------------------------------------------------*
045900970526     C     FUNCON        BEGSR
046000970526     C*
046100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
046200970526     C                   EXSR      FUNVAR
046300970526     C*
046400970526     C* PULISCO CAMPI DI LAVORO
046500970526     C                   Z-ADD     0             I                 5 0
046600970526     C                   Z-ADD     0             W                 5 0
046700970526     C*
046800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
046900970526     C     FUNKEY        IFNE      *BLANKS
047000970526     C     1             DO        24            I
047100970526     C                   MOVEL     FUD(I)        FUDRIG
047200970526     C*
047300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
047400970526     C     FUNKEY        IFEQ      FUDFUN
047500970526     C     FUDATT        ANDEQ     'S'
047600970526     C                   Z-ADD     I             W
047700970526     C                   END
047800970526     C                   END                                                    DO WHILE
047900970526     C*
048000970526     C* FUNZIONE NON ATTIVA (ERRORE)
048100970526     C     W             IFEQ      0
048200970526     C*
048300970526     C* EMISSIONE MESSAGGIO DI ERRORE
048400970526     C                   MOVEL     MSG(1)        DSMSMS
048500970526     C*
048600970526     C                   END                                                    W=0
048700970526     C                   END                                                    FUNKEY NE *BLANKS
048800970526     C*
048900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
049000970526     C     W             IFNE      0
049100970526     C     FUNKEY        OREQ      *BLANKS
049200970526     C     FUNKEY        IFEQ      *BLANKS
049300970526     C                   EXSR      ENTGES
049400970526     C                   END
049500970526     C     FUNKEY        IFEQ      '03'
049600970526     C                   EXSR      F03GES
049700970526     C                   END
049800970526     C     FUNKEY        IFEQ      '06'
049900970526     C                   EXSR      F06GES
050000970526     C                   END
050100970526     C     FUNKEY        IFEQ      '12'
050200970526     C                   EXSR      F12GES
050300970526     C                   END
050400970526     C     FUNKEY        IFEQ      '24'
050500970526     C                   EXSR      F24GES
050600970526     C                   END
050700970526     C                   END
050800970526     C*
050900970526     C                   ENDSR
051000970526     C*------------------------------------------------------------------------*
051100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051200970526     C*------------------------------------------------------------------------*
051300970526     C     FUNVAR        BEGSR
051400970526     C*
051500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
051600970526     C*
051700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
051800970526     C     KEY           IFEQ      ENTER
051900970526     C                   MOVEL     *BLANKS       FUNKEY
052000970526     C                   END
052100970526     C     KEY           IFEQ      F1
052200970526     C                   MOVEL     '01'          FUNKEY
052300970526     C                   END
052400970526     C     KEY           IFEQ      F2
052500970526     C                   MOVEL     '02'          FUNKEY
052600970526     C                   END
052700970526     C     KEY           IFEQ      F3
052800970526     C                   MOVEL     '03'          FUNKEY
052900970526     C                   END
053000970526     C     KEY           IFEQ      F4
053100970526     C                   MOVEL     '04'          FUNKEY
053200970526     C                   END
053300970526     C     KEY           IFEQ      F5
053400970526     C                   MOVEL     '05'          FUNKEY
053500970526     C                   END
053600970526     C     KEY           IFEQ      F6
053700970526     C                   MOVEL     '06'          FUNKEY
053800970526     C                   END
053900970526     C     KEY           IFEQ      F7
054000970526     C                   MOVEL     '07'          FUNKEY
054100970526     C                   END
054200970526     C     KEY           IFEQ      F8
054300970526     C                   MOVEL     '08'          FUNKEY
054400970526     C                   END
054500970526     C     KEY           IFEQ      F9
054600970526     C                   MOVEL     '09'          FUNKEY
054700970526     C                   END
054800970526     C     KEY           IFEQ      F10
054900970526     C                   MOVEL     '10'          FUNKEY
055000970526     C                   END
055100970526     C     KEY           IFEQ      F11
055200970526     C                   MOVEL     '11'          FUNKEY
055300970526     C                   END
055400970526     C     KEY           IFEQ      F12
055500970526     C                   MOVEL     '12'          FUNKEY
055600970526     C                   END
055700970526     C     KEY           IFEQ      F13
055800970526     C                   MOVEL     '13'          FUNKEY
055900970526     C                   END
056000970526     C     KEY           IFEQ      F14
056100970526     C                   MOVEL     '14'          FUNKEY
056200970526     C                   END
056300970526     C     KEY           IFEQ      F15
056400970526     C                   MOVEL     '15'          FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F16
056700970526     C                   MOVEL     '16'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F17
057000970526     C                   MOVEL     '17'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F18
057300970526     C                   MOVEL     '18'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F19
057600970526     C                   MOVEL     '19'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F20
057900970526     C                   MOVEL     '20'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F21
058200970526     C                   MOVEL     '21'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F22
058500970526     C                   MOVEL     '22'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F23
058800970526     C                   MOVEL     '23'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F24
059100970526     C                   MOVEL     '24'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      ROLLUP
059400970526     C                   MOVEL     'RU'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      HELP
059700970526     C                   MOVEL     'HP'          FUNKEY
059800970526     C                   END
059900970526     C                   ENDSR
060000970526     C*------------------------------------------------------------------------*
060100970526     C* NOTE COSTRUZIONE SCHIERE
060200970526     C*------------------------------------------------------------------------*
060300970526     C*
060400970526     C* -MSG-MESSAGGI
060500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060600970526     C* (NON TOCCARE MSG,1 E MSG,2)
060700970526     C*
060800970526     C* -CMD-COMANDI
060900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061000970526     C*
061100970526     C* -OPD-OPZIONI
061200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
061300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
061400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
061500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
061800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
061900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
062000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
062100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
062200970526     C*
062300970526     C* -FUD-FUNZIONI
062400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
062500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
062800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
063000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
063100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
063200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
063300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
063400970526     O*------------------------------------------------------------------------*
063500970526** MSG - MESSAGGI
063600020211TIS7C5R- Tasto funzionale non gestito                                          1
063700020710TIS7C5R- Tipo legame indicato errato o non presente n tabella.                 2
063800020211TIS7C5R-                                                                       3
063900020211TIS7C5R-                                                                       4
064000020211TIS7C5R-                                                                       5
064100020211TIS7C5R-                                                                       6
064200020211TIS7C5R-                                                                       7
064300020211TIS7C5R-                                                                       8
064400020211TIS7C5R-                                                                       9
064500020211TIS7C5R-                                                                       10
064600020211TIS7C5R-                                                                       11
064700020211TIS7C5R-                                                                       12
064800020211TIS7C5R-                                                                       13
064900020211TIS7C5R-                                                                       14
065000020211TIS7C5R-                                                                       15
065100020211TIS7C5R-                                                                       16
065200020211TIS7C5R-                                                                       17
065300020211TIS7C5R-                                                                       18
065400020211TIS7C5R-                                                                       19
065500020211TIS7C5R-                                                                       20
065600020211TIS7C5R-                                                                       21
065700020211TIS7C5R-                                                                       22
065800020211TIS7C5R-                                                                       23
065900020211TIS7C5R-                                                                       24
066000970526** FUD - FUNZIONI
066100970526                   -        (S/N)=N                        funz.01¬
066200970526                   -        (S/N)=N                        funz.02¬
066300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
066400970526                   -        (S/N)=N                        funz.04¬
066500970526                   -        (S/N)=N                        funz.05¬
066600991214F06=Conferma       -        (S/N)=S                        funz.06¬
066700970527                   -        (S/N)=N                        funz.07¬
066800970527                   -        (S/N)=N                        funz.08¬
066900970527                   -        (S/N)=N                        funz.09¬
067000970527                   -        (S/N)=N                        funz.10¬
067100970526                   -        (S/N)=N                        funz.11¬
067200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
067300970526                   -        (S/N)=N                        funz.13¬
067400970526                   -        (S/N)=N                        funz.14¬
067500970526                   -        (S/N)=N                        funz.15¬
067600970526                   -        (S/N)=N                        funz.16¬
067700970526                   -        (S/N)=N                        funz.17¬
067800970526                   -        (S/N)=N                        funz.18¬
067900970526                   -        (S/N)=N                        funz.19¬
068000970526                   -        (S/N)=N                        funz.20¬
068100970526                   -        (S/N)=N                        funz.21¬
068200970526                   -        (S/N)=N                        funz.22¬
068300970526                   -        (S/N)=N                        funz.23¬
068400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

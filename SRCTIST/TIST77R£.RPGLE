000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060411     Ftist77d   cf   e             WORKSTN INFDS(DEVDS1)
000900060411     F                                     SFILE(st77DMS:MSGULT)
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
004800970528     D*------------------
004900970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000970528     D*------------------
005100970528     D WLBDA8          DS                  INZ
005200970528     D  G08DAT                 1      8  0
005300970528     D  G08INV                 9     16  0
005400970528     D  G08ERR                17     17
005500970528     D  G08TGI                18     22  0
005600970526     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000991214     D*------------------
006100991214     D* DS X PASSAGGIO PARAMETRI
006200991214     D*------------------
006300991214     D PARAM           DS                  INZ
006400060411     D  PARDAD                        8  0
006500060411     D  PARDAA                        8  0
006600060411     D  PARTBL1                       2
006700060411     D  PARTBL2                       2
006800060411     D  PARTBL3                       2
006900060411     D  PARTBL4                       2
007000060411     D  PARTBL5                       2
007100060411     D  PARLNA1                       3  0
007200060411     D  PARLNA2                       3  0
007300060411     D  PARLNA3                       3  0
007400060411     D  PARLNA4                       3  0
007500060411     D  PARLNA5                       3  0
007600020923     D*-------------------
007700020923     D* DS REPERIMENTO DATI UTENTE
007800020923     D*-------------------
007900020923     D TIBS34DS      E DS                                                       *Profili utente
008000020923     D DDATIUTE      E DS                                                       *Dati utente
008100020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008200970526     C*------------------------------------------------------------------------*
008300000303     C* MAIN LINES
008400970526     C*------------------------------------------------------------------------*
008500970526     C*
008600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009000970526     C*
009100970526     C* EMETTE FORMATO VIDEO
009200970526     C                   EXSR      BUTFMR
009300970526     C*
009400970526     C* GESTIONE TASTI FUNZIONALI
009500970526     C                   EXSR      FUNCON
009600970526E   2C                   ENDDO
009700970526     C*
009800970526     C* FINE PGM
009900970526     C                   SETON                                        LR
010000970526     C*------------------------------------------------------------------------*
010100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010200970526     C*------------------------------------------------------------------------*
010300970526     C     BUTFMR        BEGSR
010400970526     C*
010500970526     C* EMETTE LA TESTATA
010600060411     C                   WRITE     st77D01
010700970526     C*
010800970526     C* SPEDISCE EVENTUALE MESSAGGIO
010900970526     C                   EXSR      SNDMSG
011000970526     C*
011100970526     C* EMETTE LE FUNZIONI
011200060411     C                   WRITE     st77D02
011300970526     C*
011400970526     C* EMETTE IL VIDEO
011500060411     C                   EXFMT     st77D03
011600970526     C*
011700991214     C* REIMPOSTA FLAG DI ERRORE
011800991214     C                   MOVEL     '0'           ERRL00
011900970526     C*
012000970526     C* REIMPOSTA INDICATORI DI ERRORE
012100020611     C                   SETOFF                                       555556
012200970526     C*
012300970526     C                   ENDSR
012400970526     C*------------------------------------------------------------------------*
012500970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012600970526     C*------------------------------------------------------------------------*
012700970526     C     SNDMSG        BEGSR
012800970526     C*
012900970526     C* PULISCE IL SFILE MESSAGGI
013000970526     C                   MOVEL     '1'           *IN40
013100060411     C                   WRITE     st77DMC
013200970526     C                   MOVEL     '0'           *IN40
013300970526     C*
013400970526     C* VISUALIZZA IL MESSAGGIO
013500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013600970526     C                   CALL      'XMSG'        XMSGPA
013700020611     C     DSMSER        IFEQ      *ZEROS
013800970526     C                   MOVEL     DSMSKE        MSGKEY
013900020611     C                   ENDIF
014000970526     C                   Z-ADD     1             MSGULT
014100060411     C                   WRITE     st77DMS
014200060411     C                   WRITE     st77DMC
014300970526     C*
014400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014500970526     C                   MOVEL     *BLANKS       DSMSMS
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* ENTGES - GESTIONE ENTER -> INVIO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     ENTGES        BEGSR
015200970526     C*
015300970526     C* CONTROLLA I CAMPI IMMESSI
015400970526     C                   EXSR      DECODI
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* F03GES - GESTIONE F03 -> FINE
015900970526     C*------------------------------------------------------------------------*
016000970526     C     F03GES        BEGSR
016100970526     C*
016200970526     C* IMPOSTA "PREMUTO F03"
016300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700991214     C* F06GES - GESTIONE F06 -> CONFERMA
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F06GES        BEGSR
017000970526     C*
017100991214     C* CONTROLLA I CAMPI IMMESSI
017200991214     C                   EXSR      DECODI
017300020611     C*
017400991214     C* ESEGUE ROUTINE DI CONFERMA
017500020611     C     ERRL00        IFEQ      '0'
017600991214     C                   EXSR      ESEGUI
017700031001     C* IMPOSTA FINE PROGRAMMA
017800031001     C                   MOVEL     '1'           FINL00
017900991214     C                   ENDIF
018000970526     C*
018100991214     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F12GES - GESTIONE F12 -> RITORNO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F12GES        BEGSR
018600970526     C*
018700970526     C* IMPOSTA "PREMUTO F12"
018800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F24GES        BEGSR
019500970526     C*
019600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019700970526     C*
019800970526     C                   ENDSR
019900991214     C*------------------------------------------------------------------------*
020000991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
020100991214     C*------------------------------------------------------------------------*
020200991214     C     PASPAR        BEGSR
020300060411     C*
020400060411     C                   CLEAR                   PARAM
020500991214     C*
020600030929     C                   Z-ADD     DADVID        PARDAD
020700030929     C                   Z-ADD     DAAVID        PARDAA
020800060411     C                   MOVEL     VIDTBL1       PARTBL1
020900060411     C                   MOVEL     VIDTBL2       PARTBL2
021000060411     C                   MOVEL     VIDTBL3       PARTBL3
021100060411     C                   MOVEL     VIDTBL4       PARTBL4
021200060411     C                   MOVEL     VIDTBL5       PARTBL5
021300060411     C                   Z-ADD     VIDLNA1       PARLNA1
021400060411     C                   Z-ADD     VIDLNA2       PARLNA2
021500060411     C                   Z-ADD     VIDLNA3       PARLNA3
021600060411     C                   Z-ADD     VIDLNA4       PARLNA4
021700060411     C                   Z-ADD     VIDLNA5       PARLNA5
021800991214     C*
021900991214     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022200970526     C*------------------------------------------------------------------------*
022300991214     C     ESEGUI        BEGSR
022400991214     C*
022500991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
022600060411     C                   MOVEL     'ST78'        KCOAZ
022700991214     C                   EXSR      PASPAR
022800991214     C                   MOVEL     PARAM         KPJBU
022900031010     C                   CALL      'BCH10'
023000060411 MOM C***                CALL      'TIST78R'
023100991214     C                   PARM                    KPJBA
023200991214     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023600970526     C*------------------------------------------------------------------------*
023700970526     C     DECODI        BEGSR
023800000306     C*
023900030929     C                   SETOFF                                       5455
024000991015     C*--------------------
024100020611     C* DATA INIZIO
024200991015     C*--------------------
024300030929     C     *LIKE         DEFINE    VIDDAD        DADVID
024400020611     C     ERRL00        IFEQ      '0'
024500030929     C                   Z-ADD     VIDDAD        WDA8
024600020611     C                   Z-ADD     *ZEROS        DA8W
024700991015     C                   EXSR      CHKDA8
024800030929     C                   Z-ADD     WDA8          VIDDAD
024900030929     C                   Z-ADD     DA8W          DADVID
025000020611     C     ERRL00        IFEQ      '1'
025100991214     C                   SETON                                        54
025200020611     C                   MOVEL     MSG(14)       DSMSMS
025300020611     C                   ENDIF
025400020611     C                   ENDIF
025500970528     C*--------------------
025600020611     C* DATA FINE
025700970528     C*--------------------
025800030929     C     *LIKE         DEFINE    VIDDAA        DAAVID
025900020611     C     ERRL00        IFEQ      '0'
026000030929     C                   Z-ADD     VIDDAA        WDA8
026100020611     C                   Z-ADD     *ZEROS        DA8W
026200970528     C                   EXSR      CHKDA8
026300030929     C                   Z-ADD     WDA8          VIDDAA
026400030929     C                   Z-ADD     DA8W          DAAVID
026500020611     C     ERRL00        IFEQ      '1'
026600991214     C                   SETON                                        55
026700970528     C                   MOVEL     MSG(14)       DSMSMS
026800020611     C                   ENDIF
026900020611     C                   ENDIF
027000020611     C*
027100970528     C* SCADENZA > DECORRENZA
027200020611     C     ERRL00        IFEQ      '0'
027300030929     C     DAAVID        IFLT      DADVID
027400020611     C                   SETON                                        5455
027500970528     C                   MOVEL     MSG(15)       DSMSMS
027600970528     C                   MOVEL     '1'           ERRL00
027700020611     C                   ENDIF
027800020611     C                   ENDIF
027900000306     C*
028000970528     C                   ENDSR
028100970528     C*------------------------------------------------------------------------*
028200970528     C* CHKDA8 - CONTROLLA LA DATA (8)
028300970528     C*------------------------------------------------------------------------*
028400970528     C     CHKDA8        BEGSR
028500970528     C*
028600030929     C     *LIKE         DEFINE    VIDDAD        WDA8
028700030929     C     *LIKE         DEFINE    VIDDAA        DA8W
028800970528     C*
028900970528     C                   Z-ADD     WDA8          G08DAT
029000970528     C                   Z-ADD     *ZEROS        G08INV
029100970528     C                   MOVEL     '0'           G08ERR
029200970528     C                   CALL      'XSRDA8'
029300970528     C                   PARM                    WLBDA8
029400970528     C                   Z-ADD     G08DAT        WDA8
029500970528     C                   Z-ADD     G08INV        DA8W
029600020611     C     G08ERR        IFEQ      '1'
029700970528     C                   MOVEL     '1'           ERRL00
029800020611     C                   ENDIF
029900970528     C*
030000970528     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
030300970526     C*------------------------------------------------------------------------*
030400970526     C     AZZVID        BEGSR
030500970526     C* CAMPI
030600030929     C                   Z-ADD     *ZEROS        VIDDAD
030700030929     C                   Z-ADD     *ZEROS        VIDDAA
030800970526     C*
030900970526     C                   ENDSR
031000970526     C*------------------------------------------------------------------------*
031100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
031200970526     C*------------------------------------------------------------------------*
031300970526     C     IMPFUN        BEGSR
031400970526     C*
031500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
031600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
031700970526     C                   EXSR      FUNGES
031800970526     C*
031900970526     C                   ENDSR
032000020923     C*--------------------------------------------------------------------------------------------*
032100020923     C* REPERISCE I DATI UTENTE
032200020923     C*--------------------------------------------------------------------------------------------*
032300020923     C     REPDATIUTE    BEGSR
032400020923     C*
032500020923     C* INIZIALIZZA VARIABILI DI WRK
032600020923     C                   CLEAR                   TIBS34DS
032700020923     C                   CLEAR                   AZUTEDS
032800020923     C                   CLEAR                   DDATIUTE
032900020923     C*
033000020923     C     *DTAARA       DEFINE    §azute        azuteds
033100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
033200020923     C                   IN(E)     *DTAARA
033300020923if  1C                   IF        %Error
033400020923     c                   EVAL      I34Tla = 'L'
033500020923     C                   CALL      'TIBS34R'
033600020923     C                   PARM                    Tibs34Ds
033700020923     C                   IN        *DTAARA
033800020923e   1C                   ENDIF
033900020923     C*
034000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
034100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
034200020923     C*
034300020923     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* *INZSR - OPERAZIONI INIZIALI
034600970526     C*------------------------------------------------------------------------*
034700970526     C     *INZSR        BEGSR
034800991217     C*
034900991217     C     *ENTRY        PLIST
035000991217     C                   PARM                    KPJBA
035100020923     C*
035200020923     C* REPERISCE I DATI DELL'UTENTE
035300020923     C                   EXSR      REPDATIUTE
035400970526     C*
035500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
035600970526     C     XMSGPA        PLIST
035700970526     C                   PARM                    DSMSMS
035800970526     C                   PARM                    DSMSPG
035900970526     C                   PARM                    DSMSKE
036000970526     C                   PARM                    DSMSER
036100970526     C*
036200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
036300970526     C                   CALL      'XKEY'
036400970526     C                   PARM                    DSKEY
036500970526     C*---
036600970526     C* VARIABILI NON RIFERITE AL DATA BASE
036700970526     C*---
036800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
036900991214     C                   MOVEL     *BLANKS       DEPOP1            3
037000970526     C*---
037100970526     C* VARIABILI DI CONTROLLO
037200970526     C*---
037300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
037400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
037500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
037600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
037700970526     C*
037800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
037900060411     C                   MOVEL     'TIst77R'     DSMSPG
038000060411     C                   MOVEL     'TIst77R'     MSGQUE
038100970526     C                   MOVEL     *BLANKS       MSGKEY
038200970526     C                   MOVEL     *BLANKS       DSMSMS
038300970528     C*
038400970528     C* CALCOLA LA DATA CORRENTE
038500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
038600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
038700970528     C                   Z-ADD     WN8           G08DAT
038800970528     C                   Z-ADD     *ZEROS        G08INV
038900970528     C                   MOVEL     '0'           G08ERR
039000970528     C                   CALL      'XSRDA8'
039100970528     C                   PARM                    WLBDA8
039200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
039300000310     C*
039400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
039500000310     C                   EXSR      AZZVID
039600970526     C*
039700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
039800970526     C                   EXSR      IMPFUN
039900970526     C*
040000970526     C                   ENDSR
040100970526     C*------------------------------------------------------------------------*
040200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
040300970526     C*------------------------------------------------------------------------*
040400970526     C     FUNGES        BEGSR
040500970526     C*
040600970526     C* PULISCO CAMPI DI LAVORO
040700970526     C                   Z-ADD     0             W                 5 0
040800970526     C*
040900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
041000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
041100970526     C     1             DO        24            I
041200970526     C                   MOVEL     FUD(I)        FUDRIG
041300970526     C*
041400970526     C* PULIZIA FUNZIONI PRECEDENTI
041500970526     C                   MOVEL     *BLANKS       FUV(I)
041600970526     C*
041700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
041800970526     C* (SALTA F24)
041900970526     C     FUDATT        IFEQ      'S'
042000970526     C     FUDFUN        ANDNE     '24'
042100970526     C                   ADD       1             W
042200970526     C                   MOVEL     FUDDES        FUV(W)
042300970526     C                   END
042400970526     C                   END                                                    DO WHILE
042500970526     C*
042600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
042700970526     C* ALTRIMENTI DISATTIVO F24
042800970526     C     1             DO        24            I
042900970526     C                   MOVEL     FUD(I)        FUDRIG
043000970526     C*
043100970526     C     FUDFUN        IFEQ      '24'
043200970526     C     W             IFGT      6
043300970526     C                   MOVEL     'S'           FUDATT
043400970526     C                   ELSE
043500970526     C                   MOVEL     *BLANKS       FUDATT
043600970526     C                   END                                                    W GT 8
043700970526     C                   MOVEL     FUDRIG        FUD(I)
043800970526     C                   END
043900970526     C                   END                                                    DO WHILE
044000970526     C*
044100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
044200970526     C     W             DIV       6             I
044300970526     C                   MVR                     Z
044400970526     C*
044500970526     C     Z             IFNE      0
044600970526     C     I             ADD       1             Z
044700970526     C                   ELSE
044800970526     C                   Z-ADD     I             Z                 5 0
044900970526     C                   END
045000970526     C*
045100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
045200970526     C                   ADD       1             FUNGRU
045300970526     C     FUNGRU        IFGT      Z
045400970526     C     FUNGRU        ORLT      1
045500970526     C                   Z-ADD     1             FUNGRU
045600970526     C                   END
045700970526     C*
045800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
045900970526     C     FUNGRU        IFEQ      1
046000970526     C                   MOVEL     FUV(1)        FUN001
046100970526     C                   MOVEL     FUV(2)        FUN002
046200970526     C                   MOVEL     FUV(3)        FUN003
046300970526     C                   MOVEL     FUV(4)        FUN004
046400970526     C                   MOVEL     FUV(5)        FUN005
046500970526     C                   MOVEL     FUV(6)        FUN006
046600970526     C                   END
046700970526     C*
046800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
046900970526     C     FUNGRU        IFEQ      2
047000970526     C                   MOVEL     FUV(7)        FUN001
047100970526     C                   MOVEL     FUV(8)        FUN002
047200970526     C                   MOVEL     FUV(9)        FUN003
047300970526     C                   MOVEL     FUV(10)       FUN004
047400970526     C                   MOVEL     FUV(11)       FUN005
047500970526     C                   MOVEL     FUV(12)       FUN006
047600970526     C                   END
047700970526     C*
047800970526     C* TERZO GRUPPO DI 6 FUNZIONI
047900970526     C     FUNGRU        IFEQ      3
048000970526     C                   MOVEL     FUV(13)       FUN001
048100970526     C                   MOVEL     FUV(14)       FUN002
048200970526     C                   MOVEL     FUV(15)       FUN003
048300970526     C                   MOVEL     FUV(16)       FUN004
048400970526     C                   MOVEL     FUV(17)       FUN005
048500970526     C                   MOVEL     FUV(18)       FUN006
048600970526     C                   END
048700970526     C*
048800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
048900970526     C     FUNGRU        IFEQ      4
049000970526     C                   MOVEL     FUV(19)       FUN001
049100970526     C                   MOVEL     FUV(20)       FUN002
049200970526     C                   MOVEL     FUV(21)       FUN003
049300970526     C                   MOVEL     FUV(22)       FUN004
049400970526     C                   MOVEL     *BLANKS       FUN005
049500970526     C                   MOVEL     *BLANKS       FUN006
049600970526     C                   END
049700970526     C*
049800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
049900970526     C                   MOVEL     *BLANKS       FUN007
050000970526     C                   MOVEL     *BLANKS       FUN008
050100970526     C*
050200970526     C     1             DO        24            I
050300970526     C                   MOVEL     FUD(I)        FUDRIG
050400970526     C     FUDFUN        IFEQ      '24'
050500970526     C     FUDATT        ANDEQ     'S'
050600970526     C                   MOVEL     FUDDES        FUN008
050700970526     C                   END
050800970526     C                   END                                                    DO
050900970526     C*
051000970526     C                   ENDSR
051100970526     C*------------------------------------------------------------------------*
051200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
051300970526     C*------------------------------------------------------------------------*
051400970526     C     FUNCON        BEGSR
051500970526     C*
051600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
051700970526     C                   EXSR      FUNVAR
051800970526     C*
051900970526     C* PULISCO CAMPI DI LAVORO
052000970526     C                   Z-ADD     0             I                 5 0
052100970526     C                   Z-ADD     0             W                 5 0
052200970526     C*
052300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
052400970526     C     FUNKEY        IFNE      *BLANKS
052500970526     C     1             DO        24            I
052600970526     C                   MOVEL     FUD(I)        FUDRIG
052700970526     C*
052800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
052900970526     C     FUNKEY        IFEQ      FUDFUN
053000970526     C     FUDATT        ANDEQ     'S'
053100970526     C                   Z-ADD     I             W
053200970526     C                   END
053300970526     C                   END                                                    DO WHILE
053400970526     C*
053500970526     C* FUNZIONE NON ATTIVA (ERRORE)
053600970526     C     W             IFEQ      0
053700970526     C*
053800970526     C* EMISSIONE MESSAGGIO DI ERRORE
053900970526     C                   MOVEL     MSG(1)        DSMSMS
054000970526     C*
054100970526     C                   END                                                    W=0
054200970526     C                   END                                                    FUNKEY NE *BLANKS
054300970526     C*
054400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
054500970526     C     W             IFNE      0
054600970526     C     FUNKEY        OREQ      *BLANKS
054700970526     C     FUNKEY        IFEQ      *BLANKS
054800970526     C                   EXSR      ENTGES
054900970526     C                   END
055000970526     C     FUNKEY        IFEQ      '03'
055100970526     C                   EXSR      F03GES
055200970526     C                   END
055300970526     C     FUNKEY        IFEQ      '06'
055400970526     C                   EXSR      F06GES
055500970526     C                   END
055600970526     C     FUNKEY        IFEQ      '12'
055700970526     C                   EXSR      F12GES
055800970526     C                   END
055900970526     C     FUNKEY        IFEQ      '24'
056000970526     C                   EXSR      F24GES
056100970526     C                   END
056200970526     C                   END
056300970526     C*
056400970526     C                   ENDSR
056500970526     C*------------------------------------------------------------------------*
056600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
056700970526     C*------------------------------------------------------------------------*
056800970526     C     FUNVAR        BEGSR
056900970526     C*
057000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
057100970526     C*
057200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
057300970526     C     KEY           IFEQ      ENTER
057400970526     C                   MOVEL     *BLANKS       FUNKEY
057500970526     C                   END
057600970526     C     KEY           IFEQ      F1
057700970526     C                   MOVEL     '01'          FUNKEY
057800970526     C                   END
057900970526     C     KEY           IFEQ      F2
058000970526     C                   MOVEL     '02'          FUNKEY
058100970526     C                   END
058200970526     C     KEY           IFEQ      F3
058300970526     C                   MOVEL     '03'          FUNKEY
058400970526     C                   END
058500970526     C     KEY           IFEQ      F4
058600970526     C                   MOVEL     '04'          FUNKEY
058700970526     C                   END
058800970526     C     KEY           IFEQ      F5
058900970526     C                   MOVEL     '05'          FUNKEY
059000970526     C                   END
059100970526     C     KEY           IFEQ      F6
059200970526     C                   MOVEL     '06'          FUNKEY
059300970526     C                   END
059400970526     C     KEY           IFEQ      F7
059500970526     C                   MOVEL     '07'          FUNKEY
059600970526     C                   END
059700970526     C     KEY           IFEQ      F8
059800970526     C                   MOVEL     '08'          FUNKEY
059900970526     C                   END
060000970526     C     KEY           IFEQ      F9
060100970526     C                   MOVEL     '09'          FUNKEY
060200970526     C                   END
060300970526     C     KEY           IFEQ      F10
060400970526     C                   MOVEL     '10'          FUNKEY
060500970526     C                   END
060600970526     C     KEY           IFEQ      F11
060700970526     C                   MOVEL     '11'          FUNKEY
060800970526     C                   END
060900970526     C     KEY           IFEQ      F12
061000970526     C                   MOVEL     '12'          FUNKEY
061100970526     C                   END
061200970526     C     KEY           IFEQ      F13
061300970526     C                   MOVEL     '13'          FUNKEY
061400970526     C                   END
061500970526     C     KEY           IFEQ      F14
061600970526     C                   MOVEL     '14'          FUNKEY
061700970526     C                   END
061800970526     C     KEY           IFEQ      F15
061900970526     C                   MOVEL     '15'          FUNKEY
062000970526     C                   END
062100970526     C     KEY           IFEQ      F16
062200970526     C                   MOVEL     '16'          FUNKEY
062300970526     C                   END
062400970526     C     KEY           IFEQ      F17
062500970526     C                   MOVEL     '17'          FUNKEY
062600970526     C                   END
062700970526     C     KEY           IFEQ      F18
062800970526     C                   MOVEL     '18'          FUNKEY
062900970526     C                   END
063000970526     C     KEY           IFEQ      F19
063100970526     C                   MOVEL     '19'          FUNKEY
063200970526     C                   END
063300970526     C     KEY           IFEQ      F20
063400970526     C                   MOVEL     '20'          FUNKEY
063500970526     C                   END
063600970526     C     KEY           IFEQ      F21
063700970526     C                   MOVEL     '21'          FUNKEY
063800970526     C                   END
063900970526     C     KEY           IFEQ      F22
064000970526     C                   MOVEL     '22'          FUNKEY
064100970526     C                   END
064200970526     C     KEY           IFEQ      F23
064300970526     C                   MOVEL     '23'          FUNKEY
064400970526     C                   END
064500970526     C     KEY           IFEQ      F24
064600970526     C                   MOVEL     '24'          FUNKEY
064700970526     C                   END
064800970526     C     KEY           IFEQ      ROLLUP
064900970526     C                   MOVEL     'RU'          FUNKEY
065000970526     C                   END
065100970526     C     KEY           IFEQ      HELP
065200970526     C                   MOVEL     'HP'          FUNKEY
065300970526     C                   END
065400970526     C                   ENDSR
065500970526     C*------------------------------------------------------------------------*
065600970526     C* NOTE COSTRUZIONE SCHIERE
065700970526     C*------------------------------------------------------------------------*
065800970526     C*
065900970526     C* -MSG-MESSAGGI
066000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
066100970526     C* (NON TOCCARE MSG,1 E MSG,2)
066200970526     C*
066300970526     C* -CMD-COMANDI
066400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
066500970526     C*
066600970526     C* -OPD-OPZIONI
066700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
066800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
066900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
067000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
067700970526     C*
067800970526     C* -FUD-FUNZIONI
067900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
068000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
068100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
068200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
068300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
068400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
068500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
068600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
068700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
068800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
068900970526     O*------------------------------------------------------------------------*
069000970526** MSG - MESSAGGI
069100060411TIST77R- Tasto funzionale non gestito                                          1
069200060411TIST77R-                                                                       2
069300060411TIST77R-                                                                       3
069400060411TIST77R-                                                                       4
069500060411TIST77R-                                                                       5
069600060411TIST77R-                                                                       6
069700060411TIST77R-                                                                       7
069800060411TIST77R-                                                                       8
069900060411TIST77R-                                                                       9
070000060411TIST77R-                                                                       10
070100060411TIST77R-                                                                       11
070200060411TIST77R-                                                                       12
070300060411TIST77R-                                                                       13
070400060411TIST77R- Data errata                                                           14
070500060411TIST77R- La data inizio deve essere maggiore della data fine                   15
070600060411TIST77R-                                                                       16
070700060411TIST77R-                                                                       17
070800060411TIST77R-                                                                       18
070900060411TIST77R-                                                                       19
071000060411TIST77R-                                                                       20
071100060411TIST77R- Scelta obbligatoria.                                                  21
071200060411TIST77R-                                                                       22
071300060411TIST77R-                                                                       23
071400060411TIST77R-                                                                       24
071500970526** FUD - FUNZIONI
071600970526                   -        (S/N)=N                        funz.01¬
071700970526                   -        (S/N)=N                        funz.02¬
071800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
071900970526                   -        (S/N)=N                        funz.04¬
072000970526                   -        (S/N)=N                        funz.05¬
072100991214F06=Conferma       -        (S/N)=S                        funz.06¬
072200970527                   -        (S/N)=N                        funz.07¬
072300970527                   -        (S/N)=N                        funz.08¬
072400970527                   -        (S/N)=N                        funz.09¬
072500970527                   -        (S/N)=N                        funz.10¬
072600970526                   -        (S/N)=N                        funz.11¬
072700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
072800970526                   -        (S/N)=N                        funz.13¬
072900970526                   -        (S/N)=N                        funz.14¬
073000970526                   -        (S/N)=N                        funz.15¬
073100970526                   -        (S/N)=N                        funz.16¬
073200970526                   -        (S/N)=N                        funz.17¬
073300970526                   -        (S/N)=N                        funz.18¬
073400970526                   -        (S/N)=N                        funz.19¬
073500970526                   -        (S/N)=N                        funz.20¬
073600970526                   -        (S/N)=N                        funz.21¬
073700970526                   -        (S/N)=N                        funz.22¬
073800970526                   -        (S/N)=N                        funz.23¬
073900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

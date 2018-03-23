000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030929     Ftist73d   cf   e             WORKSTN INFDS(DEVDS1)
000900030929     F                                     SFILE(st73DMS:MSGULT)
001000071105     F                                     USROPN
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
004900970528     D*------------------
005000970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100970528     D*------------------
005200970528     D WLBDA8          DS                  INZ
005300970528     D  G08DAT                 1      8  0
005400970528     D  G08INV                 9     16  0
005500970528     D  G08ERR                17     17
005600970528     D  G08TGI                18     22  0
005700970526     D*------------
005800970526     D* ARCHITETTURA
005900970526     D*------------
006000970526     D KPJBA         E DS
006100991214     D*------------------
006200991214     D* DS X PASSAGGIO PARAMETRI
006300991214     D*------------------
006400991214     D PARAM           DS                  INZ
006500071105     D  PARTIP                        2
006600070323     D  PARTID                        1
006700070323     D  PARDAD                        8  0
006800070323     D  PARDAA                        8  0
006900020923     D*-------------------
007000020923     D* DS REPERIMENTO DATI UTENTE
007100020923     D*-------------------
007200020923     D TIBS34DS      E DS                                                       *Profili utente
007300020923     D DDATIUTE      E DS                                                       *Dati utente
007400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007500970526     C*------------------------------------------------------------------------*
007600000303     C* MAIN LINES
007700970526     C*------------------------------------------------------------------------*
007800071105     C*
007900071105     C* VERIFICO PARAMETRI KPJBU PRE-IMPOSTATA SU AZIONE DI LANCIO (SE FILE O STAMPA)
008000071105     C                   IF        PARTIP = 'ST'
008100071105     C                   CALL      'TIST73R1'
008200071105     C                   PARM                    KPJBA
008300071105     C                   ELSE
008400071105     C                   OPEN      TIST73D
008500071105     C*
008600071105     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
008700071105     C                   EXSR      AZZVID
008800071105     C*
008900071105     C* IMPOSTA LE FUNZIONI DI INGRESSO
009000071105     C                   EXSR      IMPFUN
009100970526     C*
009200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009600970526     C*
009700970526     C* EMETTE FORMATO VIDEO
009800970526     C                   EXSR      BUTFMR
009900970526     C*
010000970526     C* GESTIONE TASTI FUNZIONALI
010100970526     C                   EXSR      FUNCON
010200970526E   2C                   ENDDO
010300071105     C*
010400071105     C                   CLOSE     TIST73D
010500071105     C                   ENDIF
010600970526     C*
010700970526     C* FINE PGM
010800970526     C                   SETON                                        LR
010900970526     C*------------------------------------------------------------------------*
011000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011100970526     C*------------------------------------------------------------------------*
011200970526     C     BUTFMR        BEGSR
011300970526     C*
011400970526     C* EMETTE LA TESTATA
011500030929     C                   WRITE     st73D01
011600970526     C*
011700970526     C* SPEDISCE EVENTUALE MESSAGGIO
011800970526     C                   EXSR      SNDMSG
011900970526     C*
012000970526     C* EMETTE LE FUNZIONI
012100030929     C                   WRITE     st73D02
012200970526     C*
012300970526     C* EMETTE IL VIDEO
012400030929     C                   EXFMT     st73D03
012500970526     C*
012600991214     C* REIMPOSTA FLAG DI ERRORE
012700991214     C                   MOVEL     '0'           ERRL00
012800970526     C*
012900970526     C* REIMPOSTA INDICATORI DI ERRORE
013000020611     C                   SETOFF                                       555556
013100970526     C*
013200970526     C                   ENDSR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     SNDMSG        BEGSR
013700970526     C*
013800970526     C* PULISCE IL SFILE MESSAGGI
013900970526     C                   MOVEL     '1'           *IN40
014000030929     C                   WRITE     st73DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600020611     C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800020611     C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000030929     C                   WRITE     st73DMS
015100030929     C                   WRITE     st73DMC
015200970526     C*
015300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970526     C                   MOVEL     *BLANKS       DSMSMS
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* ENTGES - GESTIONE ENTER -> INVIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     ENTGES        BEGSR
016100970526     C*
016200970526     C* CONTROLLA I CAMPI IMMESSI
016300970526     C                   EXSR      DECODI
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F03GES - GESTIONE F03 -> FINE
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F03GES        BEGSR
017000970526     C*
017100970526     C* IMPOSTA "PREMUTO F03"
017200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600991214     C* F06GES - GESTIONE F06 -> CONFERMA
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F06GES        BEGSR
017900970526     C*
018000991214     C* CONTROLLA I CAMPI IMMESSI
018100991214     C                   EXSR      DECODI
018200020611     C*
018300991214     C* ESEGUE ROUTINE DI CONFERMA
018400020611     C     ERRL00        IFEQ      '0'
018500991214     C                   EXSR      ESEGUI
018600031001     C* IMPOSTA FINE PROGRAMMA
018700031001     C                   MOVEL     '1'           FINL00
018800991214     C                   ENDIF
018900970526     C*
019000991214     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F12GES - GESTIONE F12 -> RITORNO
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F12GES        BEGSR
019500970526     C*
019600970526     C* IMPOSTA "PREMUTO F12"
019700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F24GES        BEGSR
020400970526     C*
020500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020600970526     C*
020700970526     C                   ENDSR
020800991214     C*------------------------------------------------------------------------*
020900991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
021000991214     C*------------------------------------------------------------------------*
021100991214     C     PASPAR        BEGSR
021200991214     C*
021300070323     C                   MOVEL     VIDTID        PARTID
021400030929     C                   Z-ADD     DADVID        PARDAD
021500030929     C                   Z-ADD     DAAVID        PARDAA
021600991214     C*
021700991214     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022000970526     C*------------------------------------------------------------------------*
022100991214     C     ESEGUI        BEGSR
022200991214     C*
022300991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
022400030929     C                   MOVEL     'ST74'        KCOAZ
022500991214     C                   EXSR      PASPAR
022600991214     C                   MOVEL     PARAM         KPJBU
022700070326     C                   CALL      'BCH10'
022800070326 MOM C***                CALL      'TIST74R'
022900991214     C                   PARM                    KPJBA
023000991214     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023400970526     C*------------------------------------------------------------------------*
023500970526     C     DECODI        BEGSR
023600000306     C*
023700030929     C                   SETOFF                                       5455
023800991015     C*--------------------
023900020611     C* DATA INIZIO
024000991015     C*--------------------
024100030929     C     *LIKE         DEFINE    VIDDAD        DADVID
024200020611     C     ERRL00        IFEQ      '0'
024300030929     C                   Z-ADD     VIDDAD        WDA8
024400020611     C                   Z-ADD     *ZEROS        DA8W
024500991015     C                   EXSR      CHKDA8
024600030929     C                   Z-ADD     WDA8          VIDDAD
024700030929     C                   Z-ADD     DA8W          DADVID
024800020611     C     ERRL00        IFEQ      '1'
024900991214     C                   SETON                                        54
025000020611     C                   MOVEL     MSG(14)       DSMSMS
025100020611     C                   ENDIF
025200020611     C                   ENDIF
025300970528     C*--------------------
025400020611     C* DATA FINE
025500970528     C*--------------------
025600030929     C     *LIKE         DEFINE    VIDDAA        DAAVID
025700020611     C     ERRL00        IFEQ      '0'
025800030929     C                   Z-ADD     VIDDAA        WDA8
025900020611     C                   Z-ADD     *ZEROS        DA8W
026000970528     C                   EXSR      CHKDA8
026100030929     C                   Z-ADD     WDA8          VIDDAA
026200030929     C                   Z-ADD     DA8W          DAAVID
026300020611     C     ERRL00        IFEQ      '1'
026400991214     C                   SETON                                        55
026500970528     C                   MOVEL     MSG(14)       DSMSMS
026600020611     C                   ENDIF
026700020611     C                   ENDIF
026800020611     C*
026900970528     C* SCADENZA > DECORRENZA
027000020611     C     ERRL00        IFEQ      '0'
027100030929     C     DAAVID        IFLT      DADVID
027200020611     C                   SETON                                        5455
027300970528     C                   MOVEL     MSG(15)       DSMSMS
027400970528     C                   MOVEL     '1'           ERRL00
027500020611     C                   ENDIF
027600020611     C                   ENDIF
027700040408     C*
027800040408     C* DECORRENZA/SCADENZA ANTECEDENTI AL 15/03/2004 (DATA INIZIO MONITORIZZAZIONE MODIFICHE BOLLE
027900040408     C* IN PARTENZA E IN ARRIVO)
028000040408     C     ERRL00        IFEQ      '0'
028100040408     C                   IF        DADVID < 20040315 OR
028200040408     C                             DAAVID < 20040315
028300040408     C                   SETON                                        5455
028400040408     C                   MOVEL     MSG(16)       DSMSMS
028500040408     C                   MOVEL     '1'           ERRL00
028600040408     C                   ENDIF
028700040408     C                   ENDIF
028800000306     C*
028900970528     C                   ENDSR
029000970528     C*------------------------------------------------------------------------*
029100970528     C* CHKDA8 - CONTROLLA LA DATA (8)
029200970528     C*------------------------------------------------------------------------*
029300970528     C     CHKDA8        BEGSR
029400970528     C*
029500030929     C     *LIKE         DEFINE    VIDDAD        WDA8
029600030929     C     *LIKE         DEFINE    VIDDAA        DA8W
029700970528     C*
029800970528     C                   Z-ADD     WDA8          G08DAT
029900970528     C                   Z-ADD     *ZEROS        G08INV
030000970528     C                   MOVEL     '0'           G08ERR
030100970528     C                   CALL      'XSRDA8'
030200970528     C                   PARM                    WLBDA8
030300970528     C                   Z-ADD     G08DAT        WDA8
030400970528     C                   Z-ADD     G08INV        DA8W
030500020611     C     G08ERR        IFEQ      '1'
030600970528     C                   MOVEL     '1'           ERRL00
030700020611     C                   ENDIF
030800970528     C*
030900970528     C                   ENDSR
031000970526     C*------------------------------------------------------------------------*
031100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
031200970526     C*------------------------------------------------------------------------*
031300970526     C     AZZVID        BEGSR
031400970526     C* CAMPI
031500070509     C                   MOVEL     'C'           VIDTID
031600030929     C                   Z-ADD     *ZEROS        VIDDAD
031700030929     C                   Z-ADD     *ZEROS        VIDDAA
031800970526     C*
031900970526     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
032200970526     C*------------------------------------------------------------------------*
032300970526     C     IMPFUN        BEGSR
032400970526     C*
032500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
032600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
032700970526     C                   EXSR      FUNGES
032800970526     C*
032900970526     C                   ENDSR
033000020923     C*--------------------------------------------------------------------------------------------*
033100020923     C* REPERISCE I DATI UTENTE
033200020923     C*--------------------------------------------------------------------------------------------*
033300020923     C     REPDATIUTE    BEGSR
033400020923     C*
033500020923     C* INIZIALIZZA VARIABILI DI WRK
033600020923     C                   CLEAR                   TIBS34DS
033700020923     C                   CLEAR                   AZUTEDS
033800020923     C                   CLEAR                   DDATIUTE
033900020923     C*
034000020923     C     *DTAARA       DEFINE    §azute        azuteds
034100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
034200020923     C                   IN(E)     *DTAARA
034300020923if  1C                   IF        %Error
034400020923     c                   EVAL      I34Tla = 'L'
034500020923     C                   CALL      'TIBS34R'
034600020923     C                   PARM                    Tibs34Ds
034700020923     C                   IN        *DTAARA
034800020923e   1C                   ENDIF
034900020923     C*
035000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
035100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
035200020923     C*
035300020923     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* *INZSR - OPERAZIONI INIZIALI
035600970526     C*------------------------------------------------------------------------*
035700970526     C     *INZSR        BEGSR
035800991217     C*
035900991217     C     *ENTRY        PLIST
036000991217     C                   PARM                    KPJBA
036100071105     C                   MOVEL(P)  KPJBU         PARAM
036200020923     C*
036300020923     C* REPERISCE I DATI DELL'UTENTE
036400020923     C                   EXSR      REPDATIUTE
036500970526     C*
036600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
036700970526     C     XMSGPA        PLIST
036800970526     C                   PARM                    DSMSMS
036900970526     C                   PARM                    DSMSPG
037000970526     C                   PARM                    DSMSKE
037100970526     C                   PARM                    DSMSER
037200970526     C*
037300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
037400970526     C                   CALL      'XKEY'
037500970526     C                   PARM                    DSKEY
037600970526     C*---
037700970526     C* VARIABILI NON RIFERITE AL DATA BASE
037800970526     C*---
037900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
038000991214     C                   MOVEL     *BLANKS       DEPOP1            3
038100970526     C*---
038200970526     C* VARIABILI DI CONTROLLO
038300970526     C*---
038400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
038500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
038600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
038700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
038800970526     C*
038900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
039000030929     C                   MOVEL     'TIST73R'     DSMSPG
039100030929     C                   MOVEL     'TIST73R'     MSGQUE
039200970526     C                   MOVEL     *BLANKS       MSGKEY
039300970526     C                   MOVEL     *BLANKS       DSMSMS
039400970528     C*
039500970528     C* CALCOLA LA DATA CORRENTE
039600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
039700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
039800970528     C                   Z-ADD     WN8           G08DAT
039900970528     C                   Z-ADD     *ZEROS        G08INV
040000970528     C                   MOVEL     '0'           G08ERR
040100970528     C                   CALL      'XSRDA8'
040200970528     C                   PARM                    WLBDA8
040300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
040800970526     C*------------------------------------------------------------------------*
040900970526     C     FUNGES        BEGSR
041000970526     C*
041100970526     C* PULISCO CAMPI DI LAVORO
041200970526     C                   Z-ADD     0             W                 5 0
041300970526     C*
041400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
041500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
041600970526     C     1             DO        24            I
041700970526     C                   MOVEL     FUD(I)        FUDRIG
041800970526     C*
041900970526     C* PULIZIA FUNZIONI PRECEDENTI
042000970526     C                   MOVEL     *BLANKS       FUV(I)
042100970526     C*
042200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
042300970526     C* (SALTA F24)
042400970526     C     FUDATT        IFEQ      'S'
042500970526     C     FUDFUN        ANDNE     '24'
042600970526     C                   ADD       1             W
042700970526     C                   MOVEL     FUDDES        FUV(W)
042800970526     C                   END
042900970526     C                   END                                                    DO WHILE
043000970526     C*
043100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
043200970526     C* ALTRIMENTI DISATTIVO F24
043300970526     C     1             DO        24            I
043400970526     C                   MOVEL     FUD(I)        FUDRIG
043500970526     C*
043600970526     C     FUDFUN        IFEQ      '24'
043700970526     C     W             IFGT      6
043800970526     C                   MOVEL     'S'           FUDATT
043900970526     C                   ELSE
044000970526     C                   MOVEL     *BLANKS       FUDATT
044100970526     C                   END                                                    W GT 8
044200970526     C                   MOVEL     FUDRIG        FUD(I)
044300970526     C                   END
044400970526     C                   END                                                    DO WHILE
044500970526     C*
044600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
044700970526     C     W             DIV       6             I
044800970526     C                   MVR                     Z
044900970526     C*
045000970526     C     Z             IFNE      0
045100970526     C     I             ADD       1             Z
045200970526     C                   ELSE
045300970526     C                   Z-ADD     I             Z                 5 0
045400970526     C                   END
045500970526     C*
045600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
045700970526     C                   ADD       1             FUNGRU
045800970526     C     FUNGRU        IFGT      Z
045900970526     C     FUNGRU        ORLT      1
046000970526     C                   Z-ADD     1             FUNGRU
046100970526     C                   END
046200970526     C*
046300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
046400970526     C     FUNGRU        IFEQ      1
046500970526     C                   MOVEL     FUV(1)        FUN001
046600970526     C                   MOVEL     FUV(2)        FUN002
046700970526     C                   MOVEL     FUV(3)        FUN003
046800970526     C                   MOVEL     FUV(4)        FUN004
046900970526     C                   MOVEL     FUV(5)        FUN005
047000970526     C                   MOVEL     FUV(6)        FUN006
047100970526     C                   END
047200970526     C*
047300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
047400970526     C     FUNGRU        IFEQ      2
047500970526     C                   MOVEL     FUV(7)        FUN001
047600970526     C                   MOVEL     FUV(8)        FUN002
047700970526     C                   MOVEL     FUV(9)        FUN003
047800970526     C                   MOVEL     FUV(10)       FUN004
047900970526     C                   MOVEL     FUV(11)       FUN005
048000970526     C                   MOVEL     FUV(12)       FUN006
048100970526     C                   END
048200970526     C*
048300970526     C* TERZO GRUPPO DI 6 FUNZIONI
048400970526     C     FUNGRU        IFEQ      3
048500970526     C                   MOVEL     FUV(13)       FUN001
048600970526     C                   MOVEL     FUV(14)       FUN002
048700970526     C                   MOVEL     FUV(15)       FUN003
048800970526     C                   MOVEL     FUV(16)       FUN004
048900970526     C                   MOVEL     FUV(17)       FUN005
049000970526     C                   MOVEL     FUV(18)       FUN006
049100970526     C                   END
049200970526     C*
049300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
049400970526     C     FUNGRU        IFEQ      4
049500970526     C                   MOVEL     FUV(19)       FUN001
049600970526     C                   MOVEL     FUV(20)       FUN002
049700970526     C                   MOVEL     FUV(21)       FUN003
049800970526     C                   MOVEL     FUV(22)       FUN004
049900970526     C                   MOVEL     *BLANKS       FUN005
050000970526     C                   MOVEL     *BLANKS       FUN006
050100970526     C                   END
050200970526     C*
050300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
050400970526     C                   MOVEL     *BLANKS       FUN007
050500970526     C                   MOVEL     *BLANKS       FUN008
050600970526     C*
050700970526     C     1             DO        24            I
050800970526     C                   MOVEL     FUD(I)        FUDRIG
050900970526     C     FUDFUN        IFEQ      '24'
051000970526     C     FUDATT        ANDEQ     'S'
051100970526     C                   MOVEL     FUDDES        FUN008
051200970526     C                   END
051300970526     C                   END                                                    DO
051400970526     C*
051500970526     C                   ENDSR
051600970526     C*------------------------------------------------------------------------*
051700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
051800970526     C*------------------------------------------------------------------------*
051900970526     C     FUNCON        BEGSR
052000970526     C*
052100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
052200970526     C                   EXSR      FUNVAR
052300970526     C*
052400970526     C* PULISCO CAMPI DI LAVORO
052500970526     C                   Z-ADD     0             I                 5 0
052600970526     C                   Z-ADD     0             W                 5 0
052700970526     C*
052800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
052900970526     C     FUNKEY        IFNE      *BLANKS
053000970526     C     1             DO        24            I
053100970526     C                   MOVEL     FUD(I)        FUDRIG
053200970526     C*
053300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
053400970526     C     FUNKEY        IFEQ      FUDFUN
053500970526     C     FUDATT        ANDEQ     'S'
053600970526     C                   Z-ADD     I             W
053700970526     C                   END
053800970526     C                   END                                                    DO WHILE
053900970526     C*
054000970526     C* FUNZIONE NON ATTIVA (ERRORE)
054100970526     C     W             IFEQ      0
054200970526     C*
054300970526     C* EMISSIONE MESSAGGIO DI ERRORE
054400970526     C                   MOVEL     MSG(1)        DSMSMS
054500970526     C*
054600970526     C                   END                                                    W=0
054700970526     C                   END                                                    FUNKEY NE *BLANKS
054800970526     C*
054900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
055000970526     C     W             IFNE      0
055100970526     C     FUNKEY        OREQ      *BLANKS
055200970526     C     FUNKEY        IFEQ      *BLANKS
055300970526     C                   EXSR      ENTGES
055400970526     C                   END
055500970526     C     FUNKEY        IFEQ      '03'
055600970526     C                   EXSR      F03GES
055700970526     C                   END
055800970526     C     FUNKEY        IFEQ      '06'
055900970526     C                   EXSR      F06GES
056000970526     C                   END
056100970526     C     FUNKEY        IFEQ      '12'
056200970526     C                   EXSR      F12GES
056300970526     C                   END
056400970526     C     FUNKEY        IFEQ      '24'
056500970526     C                   EXSR      F24GES
056600970526     C                   END
056700970526     C                   END
056800970526     C*
056900970526     C                   ENDSR
057000970526     C*------------------------------------------------------------------------*
057100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
057200970526     C*------------------------------------------------------------------------*
057300970526     C     FUNVAR        BEGSR
057400970526     C*
057500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
057600970526     C*
057700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
057800970526     C     KEY           IFEQ      ENTER
057900970526     C                   MOVEL     *BLANKS       FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F1
058200970526     C                   MOVEL     '01'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F2
058500970526     C                   MOVEL     '02'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F3
058800970526     C                   MOVEL     '03'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F4
059100970526     C                   MOVEL     '04'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F5
059400970526     C                   MOVEL     '05'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F6
059700970526     C                   MOVEL     '06'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F7
060000970526     C                   MOVEL     '07'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F8
060300970526     C                   MOVEL     '08'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F9
060600970526     C                   MOVEL     '09'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F10
060900970526     C                   MOVEL     '10'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F11
061200970526     C                   MOVEL     '11'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F12
061500970526     C                   MOVEL     '12'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F13
061800970526     C                   MOVEL     '13'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F14
062100970526     C                   MOVEL     '14'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F15
062400970526     C                   MOVEL     '15'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F16
062700970526     C                   MOVEL     '16'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F17
063000970526     C                   MOVEL     '17'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F18
063300970526     C                   MOVEL     '18'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F19
063600970526     C                   MOVEL     '19'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F20
063900970526     C                   MOVEL     '20'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F21
064200970526     C                   MOVEL     '21'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F22
064500970526     C                   MOVEL     '22'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F23
064800970526     C                   MOVEL     '23'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F24
065100970526     C                   MOVEL     '24'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      ROLLUP
065400970526     C                   MOVEL     'RU'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      HELP
065700970526     C                   MOVEL     'HP'          FUNKEY
065800970526     C                   END
065900970526     C                   ENDSR
066000970526     C*------------------------------------------------------------------------*
066100970526     C* NOTE COSTRUZIONE SCHIERE
066200970526     C*------------------------------------------------------------------------*
066300970526     C*
066400970526     C* -MSG-MESSAGGI
066500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
066600970526     C* (NON TOCCARE MSG,1 E MSG,2)
066700970526     C*
066800970526     C* -CMD-COMANDI
066900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
067000970526     C*
067100970526     C* -OPD-OPZIONI
067200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
067300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
067400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
067500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
068000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
068100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
068200970526     C*
068300970526     C* -FUD-FUNZIONI
068400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
068500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
068600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
068700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
068800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
068900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
069100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
069200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
069300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
069400970526     O*------------------------------------------------------------------------*
069500970526** MSG - MESSAGGI
069600030929TIST73R- Tasto funzionale non gestito                                          1
069700030929TIST73R-                                                                       2
069800030929TIST73R-                                                                       3
069900030929TIST73R-                                                                       4
070000030929TIST73R-                                                                       5
070100030929TIST73R-                                                                       6
070200030929TIST73R-                                                                       7
070300030929TIST73R-                                                                       8
070400030929TIST73R-                                                                       9
070500030929TIST73R-                                                                       10
070600030929TIST73R-                                                                       11
070700030929TIST73R-                                                                       12
070800030929TIST73R-                                                                       13
070900030929TIST73R- Data errata                                                           14
071000030929TIST73R- La data inizio deve essere maggiore della data fine                   15
071100040408TIST73R- Non è possibile richiedere un periodo antecedente il 15/03/2004.      16
071200030929TIST73R-                                                                       17
071300030929TIST73R-                                                                       18
071400030929TIST73R-                                                                       19
071500030929TIST73R-                                                                       20
071600030929TIST73R- Scelta obbligatoria.                                                  21
071700030929TIST73R-                                                                       22
071800030929TIST73R-                                                                       23
071900030929TIST73R-                                                                       24
072000970526** FUD - FUNZIONI
072100970526                   -        (S/N)=N                        funz.01¬
072200970526                   -        (S/N)=N                        funz.02¬
072300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
072400970526                   -        (S/N)=N                        funz.04¬
072500970526                   -        (S/N)=N                        funz.05¬
072600991214F06=Conferma       -        (S/N)=S                        funz.06¬
072700970527                   -        (S/N)=N                        funz.07¬
072800970527                   -        (S/N)=N                        funz.08¬
072900970527                   -        (S/N)=N                        funz.09¬
073000970527                   -        (S/N)=N                        funz.10¬
073100970526                   -        (S/N)=N                        funz.11¬
073200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
073300970526                   -        (S/N)=N                        funz.13¬
073400970526                   -        (S/N)=N                        funz.14¬
073500970526                   -        (S/N)=N                        funz.15¬
073600970526                   -        (S/N)=N                        funz.16¬
073700970526                   -        (S/N)=N                        funz.17¬
073800970526                   -        (S/N)=N                        funz.18¬
073900970526                   -        (S/N)=N                        funz.19¬
074000970526                   -        (S/N)=N                        funz.20¬
074100970526                   -        (S/N)=N                        funz.21¬
074200970526                   -        (S/N)=N                        funz.22¬
074300970526                   -        (S/N)=N                        funz.23¬
074400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

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
019100140423     C*------------------------------------------------------------------------*
019200140423     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
019300140423     C*------------------------------------------------------------------------*
019400140429     C***  F09GES        BEGSR
019500140423     C*
019600140429     C**                 MOVE      '1'           KRITB
019700140429     C**                 MOVEL     'ST74'        KCOAZ
019800140423     c
019900140429     c**                 call      'BCH09'
020000140429     c**                 parm                    kpjba
020100140423     C*
020200140429     C***                ENDSR
020300970526     C*------------------------------------------------------------------------*
020400970526     C* F12GES - GESTIONE F12 -> RITORNO
020500970526     C*------------------------------------------------------------------------*
020600970526     C     F12GES        BEGSR
020700970526     C*
020800970526     C* IMPOSTA "PREMUTO F12"
020900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F24GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021800970526     C*
021900970526     C                   ENDSR
022000991214     C*------------------------------------------------------------------------*
022100991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
022200991214     C*------------------------------------------------------------------------*
022300991214     C     PASPAR        BEGSR
022400991214     C*
022500070323     C                   MOVEL     VIDTID        PARTID
022600030929     C                   Z-ADD     DADVID        PARDAD
022700030929     C                   Z-ADD     DAAVID        PARDAA
022800991214     C*
022900991214     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
023200970526     C*------------------------------------------------------------------------*
023300991214     C     ESEGUI        BEGSR
023400991214     C*
023500991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
023600030929     C                   MOVEL     'ST74'        KCOAZ
023700991214     C                   EXSR      PASPAR
023800991214     C                   MOVEL     PARAM         KPJBU
023900140429     c                   If        *In12
024000140429     c                   call      'BCH09'
024100140429     c                   parm                    kpjba
024200140429     c                   EndIf
024300070326     C                   CALL      'BCH10'
024400070326 MOM C***                CALL      'TIST74R'
024500991214     C                   PARM                    KPJBA
024600991214     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025000970526     C*------------------------------------------------------------------------*
025100970526     C     DECODI        BEGSR
025200000306     C*
025300030929     C                   SETOFF                                       5455
025400991015     C*--------------------
025500020611     C* DATA INIZIO
025600991015     C*--------------------
025700030929     C     *LIKE         DEFINE    VIDDAD        DADVID
025800020611     C     ERRL00        IFEQ      '0'
025900030929     C                   Z-ADD     VIDDAD        WDA8
026000020611     C                   Z-ADD     *ZEROS        DA8W
026100991015     C                   EXSR      CHKDA8
026200030929     C                   Z-ADD     WDA8          VIDDAD
026300030929     C                   Z-ADD     DA8W          DADVID
026400020611     C     ERRL00        IFEQ      '1'
026500991214     C                   SETON                                        54
026600020611     C                   MOVEL     MSG(14)       DSMSMS
026700020611     C                   ENDIF
026800020611     C                   ENDIF
026900970528     C*--------------------
027000020611     C* DATA FINE
027100970528     C*--------------------
027200030929     C     *LIKE         DEFINE    VIDDAA        DAAVID
027300020611     C     ERRL00        IFEQ      '0'
027400030929     C                   Z-ADD     VIDDAA        WDA8
027500020611     C                   Z-ADD     *ZEROS        DA8W
027600970528     C                   EXSR      CHKDA8
027700030929     C                   Z-ADD     WDA8          VIDDAA
027800030929     C                   Z-ADD     DA8W          DAAVID
027900020611     C     ERRL00        IFEQ      '1'
028000991214     C                   SETON                                        55
028100970528     C                   MOVEL     MSG(14)       DSMSMS
028200020611     C                   ENDIF
028300020611     C                   ENDIF
028400020611     C*
028500970528     C* SCADENZA > DECORRENZA
028600020611     C     ERRL00        IFEQ      '0'
028700030929     C     DAAVID        IFLT      DADVID
028800020611     C                   SETON                                        5455
028900970528     C                   MOVEL     MSG(15)       DSMSMS
029000970528     C                   MOVEL     '1'           ERRL00
029100020611     C                   ENDIF
029200020611     C                   ENDIF
029300040408     C*
029400040408     C* DECORRENZA/SCADENZA ANTECEDENTI AL 15/03/2004 (DATA INIZIO MONITORIZZAZIONE MODIFICHE BOLLE
029500040408     C* IN PARTENZA E IN ARRIVO)
029600040408     C     ERRL00        IFEQ      '0'
029700040408     C                   IF        DADVID < 20040315 OR
029800040408     C                             DAAVID < 20040315
029900040408     C                   SETON                                        5455
030000040408     C                   MOVEL     MSG(16)       DSMSMS
030100040408     C                   MOVEL     '1'           ERRL00
030200040408     C                   ENDIF
030300040408     C                   ENDIF
030400000306     C*
030500970528     C                   ENDSR
030600970528     C*------------------------------------------------------------------------*
030700970528     C* CHKDA8 - CONTROLLA LA DATA (8)
030800970528     C*------------------------------------------------------------------------*
030900970528     C     CHKDA8        BEGSR
031000970528     C*
031100030929     C     *LIKE         DEFINE    VIDDAD        WDA8
031200030929     C     *LIKE         DEFINE    VIDDAA        DA8W
031300970528     C*
031400970528     C                   Z-ADD     WDA8          G08DAT
031500970528     C                   Z-ADD     *ZEROS        G08INV
031600970528     C                   MOVEL     '0'           G08ERR
031700970528     C                   CALL      'XSRDA8'
031800970528     C                   PARM                    WLBDA8
031900970528     C                   Z-ADD     G08DAT        WDA8
032000970528     C                   Z-ADD     G08INV        DA8W
032100020611     C     G08ERR        IFEQ      '1'
032200970528     C                   MOVEL     '1'           ERRL00
032300020611     C                   ENDIF
032400970528     C*
032500970528     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
032800970526     C*------------------------------------------------------------------------*
032900970526     C     AZZVID        BEGSR
033000970526     C* CAMPI
033100070509     C                   MOVEL     'C'           VIDTID
033200030929     C                   Z-ADD     *ZEROS        VIDDAD
033300030929     C                   Z-ADD     *ZEROS        VIDDAA
033400970526     C*
033500970526     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
033800970526     C*------------------------------------------------------------------------*
033900970526     C     IMPFUN        BEGSR
034000970526     C*
034100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
034200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
034300970526     C                   EXSR      FUNGES
034400970526     C*
034500970526     C                   ENDSR
034600020923     C*--------------------------------------------------------------------------------------------*
034700020923     C* REPERISCE I DATI UTENTE
034800020923     C*--------------------------------------------------------------------------------------------*
034900020923     C     REPDATIUTE    BEGSR
035000020923     C*
035100020923     C* INIZIALIZZA VARIABILI DI WRK
035200020923     C                   CLEAR                   TIBS34DS
035300020923     C                   CLEAR                   AZUTEDS
035400020923     C                   CLEAR                   DDATIUTE
035500020923     C*
035600020923     C     *DTAARA       DEFINE    §azute        azuteds
035700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
035800020923     C                   IN(E)     *DTAARA
035900020923if  1C                   IF        %Error
036000020923     c                   EVAL      I34Tla = 'L'
036100020923     C                   CALL      'TIBS34R'
036200020923     C                   PARM                    Tibs34Ds
036300020923     C                   IN        *DTAARA
036400020923e   1C                   ENDIF
036500140429     c
036600140429     c                   IF        simfel = *zeros
036700140429     c                   eval      *in12 = *on
036800140429e   1C                   ENDIF
036900020923     C*
037000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
037200020923     C*
037300020923     C                   ENDSR
037400970526     C*------------------------------------------------------------------------*
037500970526     C* *INZSR - OPERAZIONI INIZIALI
037600970526     C*------------------------------------------------------------------------*
037700970526     C     *INZSR        BEGSR
037800991217     C*
037900991217     C     *ENTRY        PLIST
038000991217     C                   PARM                    KPJBA
038100071105     C                   MOVEL(P)  KPJBU         PARAM
038200020923     C*
038300020923     C* REPERISCE I DATI DELL'UTENTE
038400020923     C                   EXSR      REPDATIUTE
038500970526     C*
038600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
038700970526     C     XMSGPA        PLIST
038800970526     C                   PARM                    DSMSMS
038900970526     C                   PARM                    DSMSPG
039000970526     C                   PARM                    DSMSKE
039100970526     C                   PARM                    DSMSER
039200970526     C*
039300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
039400970526     C                   CALL      'XKEY'
039500970526     C                   PARM                    DSKEY
039600140423     c* imposto il tasto funzionale F9 solo se sono in sede
039700140429     c**                 if        %subst(knsif:1:6)='GAITRA'
039800140429     C**                 MOVEL     FUD(09)       FUDRIG                         *SINONIMI
039900140429     C**                 MOVEL     'S'           FUDATT
040000140429     C**                 MOVEL     FUDRIG        FUD(09)
040100140429     c**                 endif
040200970526     C*---
040300970526     C* VARIABILI NON RIFERITE AL DATA BASE
040400970526     C*---
040500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
040600991214     C                   MOVEL     *BLANKS       DEPOP1            3
040700970526     C*---
040800970526     C* VARIABILI DI CONTROLLO
040900970526     C*---
041000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
041100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
041200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
041300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
041400970526     C*
041500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
041600030929     C                   MOVEL     'TIST73R'     DSMSPG
041700030929     C                   MOVEL     'TIST73R'     MSGQUE
041800970526     C                   MOVEL     *BLANKS       MSGKEY
041900970526     C                   MOVEL     *BLANKS       DSMSMS
042000970528     C*
042100970528     C* CALCOLA LA DATA CORRENTE
042200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
042300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
042400970528     C                   Z-ADD     WN8           G08DAT
042500970528     C                   Z-ADD     *ZEROS        G08INV
042600970528     C                   MOVEL     '0'           G08ERR
042700970528     C                   CALL      'XSRDA8'
042800970528     C                   PARM                    WLBDA8
042900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043000970526     C*
043100970526     C                   ENDSR
043200970526     C*------------------------------------------------------------------------*
043300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
043400970526     C*------------------------------------------------------------------------*
043500970526     C     FUNGES        BEGSR
043600970526     C*
043700970526     C* PULISCO CAMPI DI LAVORO
043800970526     C                   Z-ADD     0             W                 5 0
043900970526     C*
044000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
044200970526     C     1             DO        24            I
044300970526     C                   MOVEL     FUD(I)        FUDRIG
044400970526     C*
044500970526     C* PULIZIA FUNZIONI PRECEDENTI
044600970526     C                   MOVEL     *BLANKS       FUV(I)
044700970526     C*
044800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
044900970526     C* (SALTA F24)
045000970526     C     FUDATT        IFEQ      'S'
045100970526     C     FUDFUN        ANDNE     '24'
045200970526     C                   ADD       1             W
045300970526     C                   MOVEL     FUDDES        FUV(W)
045400970526     C                   END
045500970526     C                   END                                                    DO WHILE
045600970526     C*
045700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
045800970526     C* ALTRIMENTI DISATTIVO F24
045900970526     C     1             DO        24            I
046000970526     C                   MOVEL     FUD(I)        FUDRIG
046100970526     C*
046200970526     C     FUDFUN        IFEQ      '24'
046300970526     C     W             IFGT      6
046400970526     C                   MOVEL     'S'           FUDATT
046500970526     C                   ELSE
046600970526     C                   MOVEL     *BLANKS       FUDATT
046700970526     C                   END                                                    W GT 8
046800970526     C                   MOVEL     FUDRIG        FUD(I)
046900970526     C                   END
047000970526     C                   END                                                    DO WHILE
047100970526     C*
047200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
047300970526     C     W             DIV       6             I
047400970526     C                   MVR                     Z
047500970526     C*
047600970526     C     Z             IFNE      0
047700970526     C     I             ADD       1             Z
047800970526     C                   ELSE
047900970526     C                   Z-ADD     I             Z                 5 0
048000970526     C                   END
048100970526     C*
048200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
048300970526     C                   ADD       1             FUNGRU
048400970526     C     FUNGRU        IFGT      Z
048500970526     C     FUNGRU        ORLT      1
048600970526     C                   Z-ADD     1             FUNGRU
048700970526     C                   END
048800970526     C*
048900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
049000970526     C     FUNGRU        IFEQ      1
049100970526     C                   MOVEL     FUV(1)        FUN001
049200970526     C                   MOVEL     FUV(2)        FUN002
049300970526     C                   MOVEL     FUV(3)        FUN003
049400970526     C                   MOVEL     FUV(4)        FUN004
049500970526     C                   MOVEL     FUV(5)        FUN005
049600970526     C                   MOVEL     FUV(6)        FUN006
049700970526     C                   END
049800970526     C*
049900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
050000970526     C     FUNGRU        IFEQ      2
050100970526     C                   MOVEL     FUV(7)        FUN001
050200970526     C                   MOVEL     FUV(8)        FUN002
050300970526     C                   MOVEL     FUV(9)        FUN003
050400970526     C                   MOVEL     FUV(10)       FUN004
050500970526     C                   MOVEL     FUV(11)       FUN005
050600970526     C                   MOVEL     FUV(12)       FUN006
050700970526     C                   END
050800970526     C*
050900970526     C* TERZO GRUPPO DI 6 FUNZIONI
051000970526     C     FUNGRU        IFEQ      3
051100970526     C                   MOVEL     FUV(13)       FUN001
051200970526     C                   MOVEL     FUV(14)       FUN002
051300970526     C                   MOVEL     FUV(15)       FUN003
051400970526     C                   MOVEL     FUV(16)       FUN004
051500970526     C                   MOVEL     FUV(17)       FUN005
051600970526     C                   MOVEL     FUV(18)       FUN006
051700970526     C                   END
051800970526     C*
051900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
052000970526     C     FUNGRU        IFEQ      4
052100970526     C                   MOVEL     FUV(19)       FUN001
052200970526     C                   MOVEL     FUV(20)       FUN002
052300970526     C                   MOVEL     FUV(21)       FUN003
052400970526     C                   MOVEL     FUV(22)       FUN004
052500970526     C                   MOVEL     *BLANKS       FUN005
052600970526     C                   MOVEL     *BLANKS       FUN006
052700970526     C                   END
052800970526     C*
052900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
053000970526     C                   MOVEL     *BLANKS       FUN007
053100970526     C                   MOVEL     *BLANKS       FUN008
053200970526     C*
053300970526     C     1             DO        24            I
053400970526     C                   MOVEL     FUD(I)        FUDRIG
053500970526     C     FUDFUN        IFEQ      '24'
053600970526     C     FUDATT        ANDEQ     'S'
053700970526     C                   MOVEL     FUDDES        FUN008
053800970526     C                   END
053900970526     C                   END                                                    DO
054000970526     C*
054100970526     C                   ENDSR
054200970526     C*------------------------------------------------------------------------*
054300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
054400970526     C*------------------------------------------------------------------------*
054500970526     C     FUNCON        BEGSR
054600970526     C*
054700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
054800970526     C                   EXSR      FUNVAR
054900970526     C*
055000970526     C* PULISCO CAMPI DI LAVORO
055100970526     C                   Z-ADD     0             I                 5 0
055200970526     C                   Z-ADD     0             W                 5 0
055300970526     C*
055400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
055500970526     C     FUNKEY        IFNE      *BLANKS
055600970526     C     1             DO        24            I
055700970526     C                   MOVEL     FUD(I)        FUDRIG
055800970526     C*
055900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
056000970526     C     FUNKEY        IFEQ      FUDFUN
056100970526     C     FUDATT        ANDEQ     'S'
056200970526     C                   Z-ADD     I             W
056300970526     C                   END
056400970526     C                   END                                                    DO WHILE
056500970526     C*
056600970526     C* FUNZIONE NON ATTIVA (ERRORE)
056700970526     C     W             IFEQ      0
056800970526     C*
056900970526     C* EMISSIONE MESSAGGIO DI ERRORE
057000970526     C                   MOVEL     MSG(1)        DSMSMS
057100970526     C*
057200970526     C                   END                                                    W=0
057300970526     C                   END                                                    FUNKEY NE *BLANKS
057400970526     C*
057500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
057600970526     C     W             IFNE      0
057700970526     C     FUNKEY        OREQ      *BLANKS
057800970526     C     FUNKEY        IFEQ      *BLANKS
057900970526     C                   EXSR      ENTGES
058000970526     C                   END
058100970526     C     FUNKEY        IFEQ      '03'
058200970526     C                   EXSR      F03GES
058300970526     C                   END
058400970526     C     FUNKEY        IFEQ      '06'
058500970526     C                   EXSR      F06GES
058600970526     C                   END
058700140423     C     FUNKEY        IFEQ      '09'
058800140429     C***                EXSR      F09GES
058900140423     C                   END
059000970526     C     FUNKEY        IFEQ      '12'
059100970526     C                   EXSR      F12GES
059200970526     C                   END
059300970526     C     FUNKEY        IFEQ      '24'
059400970526     C                   EXSR      F24GES
059500970526     C                   END
059600970526     C                   END
059700970526     C*
059800970526     C                   ENDSR
059900970526     C*------------------------------------------------------------------------*
060000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060100970526     C*------------------------------------------------------------------------*
060200970526     C     FUNVAR        BEGSR
060300970526     C*
060400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
060500970526     C*
060600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
060700970526     C     KEY           IFEQ      ENTER
060800970526     C                   MOVEL     *BLANKS       FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F1
061100970526     C                   MOVEL     '01'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      F2
061400970526     C                   MOVEL     '02'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      F3
061700970526     C                   MOVEL     '03'          FUNKEY
061800970526     C                   END
061900970526     C     KEY           IFEQ      F4
062000970526     C                   MOVEL     '04'          FUNKEY
062100970526     C                   END
062200970526     C     KEY           IFEQ      F5
062300970526     C                   MOVEL     '05'          FUNKEY
062400970526     C                   END
062500970526     C     KEY           IFEQ      F6
062600970526     C                   MOVEL     '06'          FUNKEY
062700970526     C                   END
062800970526     C     KEY           IFEQ      F7
062900970526     C                   MOVEL     '07'          FUNKEY
063000970526     C                   END
063100970526     C     KEY           IFEQ      F8
063200970526     C                   MOVEL     '08'          FUNKEY
063300970526     C                   END
063400970526     C     KEY           IFEQ      F9
063500970526     C                   MOVEL     '09'          FUNKEY
063600970526     C                   END
063700970526     C     KEY           IFEQ      F10
063800970526     C                   MOVEL     '10'          FUNKEY
063900970526     C                   END
064000970526     C     KEY           IFEQ      F11
064100970526     C                   MOVEL     '11'          FUNKEY
064200970526     C                   END
064300970526     C     KEY           IFEQ      F12
064400970526     C                   MOVEL     '12'          FUNKEY
064500970526     C                   END
064600970526     C     KEY           IFEQ      F13
064700970526     C                   MOVEL     '13'          FUNKEY
064800970526     C                   END
064900970526     C     KEY           IFEQ      F14
065000970526     C                   MOVEL     '14'          FUNKEY
065100970526     C                   END
065200970526     C     KEY           IFEQ      F15
065300970526     C                   MOVEL     '15'          FUNKEY
065400970526     C                   END
065500970526     C     KEY           IFEQ      F16
065600970526     C                   MOVEL     '16'          FUNKEY
065700970526     C                   END
065800970526     C     KEY           IFEQ      F17
065900970526     C                   MOVEL     '17'          FUNKEY
066000970526     C                   END
066100970526     C     KEY           IFEQ      F18
066200970526     C                   MOVEL     '18'          FUNKEY
066300970526     C                   END
066400970526     C     KEY           IFEQ      F19
066500970526     C                   MOVEL     '19'          FUNKEY
066600970526     C                   END
066700970526     C     KEY           IFEQ      F20
066800970526     C                   MOVEL     '20'          FUNKEY
066900970526     C                   END
067000970526     C     KEY           IFEQ      F21
067100970526     C                   MOVEL     '21'          FUNKEY
067200970526     C                   END
067300970526     C     KEY           IFEQ      F22
067400970526     C                   MOVEL     '22'          FUNKEY
067500970526     C                   END
067600970526     C     KEY           IFEQ      F23
067700970526     C                   MOVEL     '23'          FUNKEY
067800970526     C                   END
067900970526     C     KEY           IFEQ      F24
068000970526     C                   MOVEL     '24'          FUNKEY
068100970526     C                   END
068200970526     C     KEY           IFEQ      ROLLUP
068300970526     C                   MOVEL     'RU'          FUNKEY
068400970526     C                   END
068500970526     C     KEY           IFEQ      HELP
068600970526     C                   MOVEL     'HP'          FUNKEY
068700970526     C                   END
068800970526     C                   ENDSR
068900970526     C*------------------------------------------------------------------------*
069000970526     C* NOTE COSTRUZIONE SCHIERE
069100970526     C*------------------------------------------------------------------------*
069200970526     C*
069300970526     C* -MSG-MESSAGGI
069400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069500970526     C* (NON TOCCARE MSG,1 E MSG,2)
069600970526     C*
069700970526     C* -CMD-COMANDI
069800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
069900970526     C*
070000970526     C* -OPD-OPZIONI
070100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
070200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
070300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
070400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
070900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
071100970526     C*
071200970526     C* -FUD-FUNZIONI
071300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
071400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
072100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
072200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
072300970526     O*------------------------------------------------------------------------*
072400970526** MSG - MESSAGGI
072500030929TIST73R- Tasto funzionale non gestito                                          1
072600030929TIST73R-                                                                       2
072700030929TIST73R-                                                                       3
072800030929TIST73R-                                                                       4
072900030929TIST73R-                                                                       5
073000030929TIST73R-                                                                       6
073100030929TIST73R-                                                                       7
073200030929TIST73R-                                                                       8
073300030929TIST73R-                                                                       9
073400030929TIST73R-                                                                       10
073500030929TIST73R-                                                                       11
073600030929TIST73R-                                                                       12
073700030929TIST73R-                                                                       13
073800030929TIST73R- Data errata                                                           14
073900030929TIST73R- La data inizio deve essere maggiore della data fine                   15
074000040408TIST73R- Non è possibile richiedere un periodo antecedente il 15/03/2004.      16
074100030929TIST73R-                                                                       17
074200030929TIST73R-                                                                       18
074300030929TIST73R-                                                                       19
074400030929TIST73R-                                                                       20
074500030929TIST73R- Scelta obbligatoria.                                                  21
074600030929TIST73R-                                                                       22
074700030929TIST73R-                                                                       23
074800030929TIST73R-                                                                       24
074900970526** FUD - FUNZIONI
075000970526                   -        (S/N)=N                        funz.01¬
075100970526                   -        (S/N)=N                        funz.02¬
075200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
075300970526                   -        (S/N)=N                        funz.04¬
075400970526                   -        (S/N)=N                        funz.05¬
075500991214F06=Conferma       -        (S/N)=S                        funz.06¬
075600970527                   -        (S/N)=N                        funz.07¬
075700970527                   -        (S/N)=N                        funz.08¬
075800140423F09=Gest.Skedulaz. -        (S/N)=N                        funz.09¬
075900970527                   -        (S/N)=N                        funz.10¬
076000970526                   -        (S/N)=N                        funz.11¬
076100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
076200970526                   -        (S/N)=N                        funz.13¬
076300970526                   -        (S/N)=N                        funz.14¬
076400970526                   -        (S/N)=N                        funz.15¬
076500970526                   -        (S/N)=N                        funz.16¬
076600970526                   -        (S/N)=N                        funz.17¬
076700970526                   -        (S/N)=N                        funz.18¬
076800970526                   -        (S/N)=N                        funz.19¬
076900970526                   -        (S/N)=N                        funz.20¬
077000970526                   -        (S/N)=N                        funz.21¬
077100970526                   -        (S/N)=N                        funz.22¬
077200970526                   -        (S/N)=N                        funz.23¬
077300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

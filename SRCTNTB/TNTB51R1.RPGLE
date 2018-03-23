000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800121127     FTNTB51D1  cf   e             WORKSTN INFDS(DEVDS1)
000900121127     F                                     SFILE(TB51DMS:MSGULT)
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
006000020923     D*-------------------
006100020923     D* DS REPERIMENTO DATI UTENTE
006200020923     D*-------------------
006300020923     D TIBS34DS      E DS                                                       *Profili utente
006400020923     D DDATIUTE      E DS                                                       *Dati utente
006500020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006600121127     D*
006700121127     D TNTB50DS      E DS
006800120928     D*-------------------
006900120928
007000121003     d Status         sds
007100121003     d  SDSpgm           *proc
007200121003     d  SDSprm           *parms
007300121003     d  SDSuser              254    263
007400121003
007500120928     d TIS7WST02R      pr                  extpgm('TIS7WST02R')
007600121016     d  Arch                               likeds(kpjba)
007700121024
007800121024     d wErrore         s              3s 0
007900121024     d TIS7WST03R      pr                  extpgm('TIS7WST02R')
008000121024     d  Errore                             like(wErrore)
008100121001     D*
008200121003     D*------------------
008300121003     D* DS X PASSAGGIO PARAMETRI
008400121003     D*------------------
008500121003     D PARAM           DS                  INZ
008600121003     d    wNrChia                     7s 0
008700121003     d    wDAD                        8s 0
008800121003     d    wDAA                        8s 0
008900121003     d    wCCM                        7s 0
009000121016     d    wSU                         1
009100121003     d    wVerTrk                     1
009200121003     d    wChkXml                     1
009300121003     d    wTipExe                     4
009400121003     d    wRmn                       15s 0
009500121003     d    wRma                       15
009600121003     d    wAAS                        4s 0
009700121003     d    wLNP                        3s 0
009800121003     d    wNRS                        2s 0
009900121003     d    wNSP                        7s 0
010000121019     d    wIdJob                     50    varying
010100121029     d    wNRS1                       2s 0
010200120928
010300970526     C*------------------------------------------------------------------------*
010400000303     C* MAIN LINES
010500970526     C*------------------------------------------------------------------------*
010600970526     C*
010700120927     C* metto i default su D03
010800120927     C                   EXSR      InzD03
010900120927     C*
011000120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011100991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011400970526     C*
011500970526     C* EMETTE FORMATO VIDEO
011600970526     C                   EXSR      BUTFMR
011700970526     C*
011800970526     C* GESTIONE TASTI FUNZIONALI
011900970526     C                   EXSR      FUNCON
012000970526E   2C                   ENDDO
012100970526     C*
012200970526     C* FINE PGM
012300970526     C                   SETON                                        LR
012400970526     C*------------------------------------------------------------------------*
012500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012600970526     C*------------------------------------------------------------------------*
012700970526     C     BUTFMR        BEGSR
012800970526     C*
012900970526     C* EMETTE LA TESTATA
013000121127     C                   WRITE     TB51D01
013100970526     C*
013200970526     C* SPEDISCE EVENTUALE MESSAGGIO
013300970526     C                   EXSR      SNDMSG
013400970526     C*
013500970526     C* EMETTE LE FUNZIONI
013600121127     C                   WRITE     TB51D02
013700970526     C*
013800970526     C* EMETTE IL VIDEO
013900121127     C                   EXFMT     TB51D03
014000970526     C*
014100991214     C* REIMPOSTA FLAG DI ERRORE
014200991214     C                   MOVEL     '0'           ERRL00
014300970526     C*
014400970526     C* REIMPOSTA INDICATORI DI ERRORE
014500020611     C                   SETOFF                                       555556
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900120927     C* InzD03 - Inizializzo D03
015000970526     C*------------------------------------------------------------------------*
015100120927     C     InzD03        BEGSR
015200970526     C*
015300120927     C*
015400120927     C                   ENDSR
015500120927     C*------------------------------------------------------------------------*
015600120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015700120927     C*------------------------------------------------------------------------*
015800120927     C     SNDMSG        BEGSR
015900120927     C*
016000970526     C* PULISCE IL SFILE MESSAGGI
016100970526     C                   MOVEL     '1'           *IN40
016200121127     C                   WRITE     TB51DMC
016300970526     C                   MOVEL     '0'           *IN40
016400970526     C*
016500970526     C* VISUALIZZA IL MESSAGGIO
016600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016700970526     C                   CALL      'XMSG'        XMSGPA
016800020611     C     DSMSER        IFEQ      *ZEROS
016900970526     C                   MOVEL     DSMSKE        MSGKEY
017000020611     C                   ENDIF
017100970526     C                   Z-ADD     1             MSGULT
017200121127     C                   WRITE     TB51DMS
017300121127     C                   WRITE     TB51DMC
017400970526     C*
017500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017600970526     C                   MOVEL     *BLANKS       DSMSMS
017700970526     C*
017800970526     C                   ENDSR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* ENTGES - GESTIONE ENTER -> INVIO
018100970526     C*------------------------------------------------------------------------*
018200970526     C     ENTGES        BEGSR
018300970526     C*
018400970526     C* CONTROLLA I CAMPI IMMESSI
018500970526     C                   EXSR      DECODI
018600970526     C*
018700970526     C                   ENDSR
018800970526     C*------------------------------------------------------------------------*
018900970526     C* F03GES - GESTIONE F03 -> FINE
019000970526     C*------------------------------------------------------------------------*
019100970526     C     F03GES        BEGSR
019200970526     C*
019300970526     C* IMPOSTA "PREMUTO F03"
019400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800991214     C* F06GES - GESTIONE F06 -> CONFERMA
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F06GES        BEGSR
020100970526     C*
020200991214     C* CONTROLLA I CAMPI IMMESSI
020300991214     C                   EXSR      DECODI
020400020611     C*
020500991214     C* ESEGUE ROUTINE DI CONFERMA
020600020611     C     ERRL00        IFEQ      '0'
020700991214     C                   EXSR      ESEGUI
020800121127     C* NON IMPOSTA FINE PROGRAMMA
020900121127     C***                   MOVEL     '1'           FINL00
021000991214     C                   ENDIF
021100970526     C*
021200991214     C                   ENDSR
021300121024     C*------------------------------------------------------------------------*
021400121024     C* F07RIP - GESTIONE F07 -> Ripristina l'esistente
021500121024     C*------------------------------------------------------------------------*
021600121024     C     F07Rip        BEGSR
021700121024     C*
021800121024     C                   EVAL      wErrore = 0
021900121024     C*
022000121024     C                   CALL      'TIS7WST03R'
022100121024     C                   PARM                    wErrore
022200121024     C*
022300121024     C*se c'è stato un errore lo segnalo, come segnalo se è andato tutto bene
022400121024     C                   IF        wErrore <> 0
022500121024     C                   EVAL      ERRL00 = '1'
022600121024     C                   MOVEL     MSG(23)       DSMSMS
022700121024     C                   ELSE
022800121024     C                   MOVEL     MSG(24)       DSMSMS
022900121024     C                   ENDIF
023000121024     C*
023100121024     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* F12GES - GESTIONE F12 -> RITORNO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     F12GES        BEGSR
023600970526     C*
023700970526     C* IMPOSTA "PREMUTO F12"
023800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024300970526     C*------------------------------------------------------------------------*
024400970526     C     F24GES        BEGSR
024500970526     C*
024600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
025100970526     C*------------------------------------------------------------------------*
025200991214     C     ESEGUI        BEGSR
025300991214     C*
025400121127     c                   clear                   tntb50ds
025500121127     c                   movel     VIDLin        b50lin
025600121127     c                   movel     VIDSIF        b50sif
025700121127     c                   movel     VIDKe1        b50ke1
025800121127     c                   movel     VIDKe2        b50ke2
025900121127     c                   MOVEL     'C'           b50opz
026000121127     c                   movel     VIDPOA        b50poa
026100121127     c                   call      'TNTB51R'
026200121127     c                   parm                    kpjba
026300121127     c                   parm                    tntb50ds
026400121127     c
026500121127     c                   EVAL      VIDErr = b50err
026600121127     c                   EVAL      VIDMsg = b50msg
026700991214     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
027100970526     C*------------------------------------------------------------------------*
027200970526     C     DECODI        BEGSR
027300000306     C*
027400120927     C*
027500970528     C                   ENDSR
027600970528     C*------------------------------------------------------------------------*
027700970528     C* CHKDA8 - CONTROLLA LA DATA (8)
027800970528     C*------------------------------------------------------------------------*
027900970528     C     CHKDA8        BEGSR
028000970528     C*
028100120927     C                   Z-ADD     WDA8          G08INV
028200120927     C                   Z-ADD     *ZEROS        G08DAT
028300120927     C                   MOVEL     '3'           G08ERR
028400970528     C                   CALL      'XSRDA8'
028500970528     C                   PARM                    WLBDA8
028600121127     C                   Z-ADD     G08DAT        WDA8              8 0
028700121127     C                   Z-ADD     G08INV        DA8W              8 0
028800020611     C     G08ERR        IFEQ      '1'
028900970528     C                   MOVEL     '1'           ERRL00
029000020611     C                   ENDIF
029100970528     C*
029200970528     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
029500970526     C*------------------------------------------------------------------------*
029600970526     C     AZZVID        BEGSR
029700970526     C* CAMPI
029800970526     C*
029900970526     C                   ENDSR
030000970526     C*------------------------------------------------------------------------*
030100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
030200970526     C*------------------------------------------------------------------------*
030300970526     C     IMPFUN        BEGSR
030400970526     C*
030500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
030600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
030700970526     C                   EXSR      FUNGES
030800970526     C*
030900970526     C                   ENDSR
031000020923     C*--------------------------------------------------------------------------------------------*
031100020923     C* REPERISCE I DATI UTENTE
031200020923     C*--------------------------------------------------------------------------------------------*
031300020923     C     REPDATIUTE    BEGSR
031400020923     C*
031500020923     C* INIZIALIZZA VARIABILI DI WRK
031600020923     C                   CLEAR                   TIBS34DS
031700020923     C                   CLEAR                   AZUTEDS
031800020923     C                   CLEAR                   DDATIUTE
031900020923     C*
032000020923     C     *DTAARA       DEFINE    §azute        azuteds
032100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
032200020923     C                   IN(E)     *DTAARA
032300020923if  1C                   IF        %Error
032400020923     c                   EVAL      I34Tla = 'L'
032500020923     C                   CALL      'TIBS34R'
032600020923     C                   PARM                    Tibs34Ds
032700020923     C                   IN        *DTAARA
032800020923e   1C                   ENDIF
032900020923     C*
033000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
033200020923     C*
033300020923     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* *INZSR - OPERAZIONI INIZIALI
033600970526     C*------------------------------------------------------------------------*
033700970526     C     *INZSR        BEGSR
033800991217     C*
033900991217     C     *ENTRY        PLIST
034000991217     C                   PARM                    KPJBA
034100020923     C*
034200020923     C* REPERISCE I DATI DELL'UTENTE
034300020923     C                   EXSR      REPDATIUTE
034400970526     C*
034500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
034600970526     C     XMSGPA        PLIST
034700970526     C                   PARM                    DSMSMS
034800970526     C                   PARM                    DSMSPG
034900970526     C                   PARM                    DSMSKE
035000970526     C                   PARM                    DSMSER
035100970526     C*
035200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
035300970526     C                   CALL      'XKEY'
035400970526     C                   PARM                    DSKEY
035500970526     C*---
035600970526     C* VARIABILI NON RIFERITE AL DATA BASE
035700970526     C*---
035800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
035900991214     C                   MOVEL     *BLANKS       DEPOP1            3
036000970526     C*---
036100970526     C* VARIABILI DI CONTROLLO
036200970526     C*---
036300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
036400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
036500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
036600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
036700970526     C*
036800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
036900121127     C                   MOVEL     'TNTB51R1'    DSMSPG
037000121127     C                   MOVEL     'TNTB51R1'    MSGQUE
037100970526     C                   MOVEL     *BLANKS       MSGKEY
037200970526     C                   MOVEL     *BLANKS       DSMSMS
037300970528     C*
037400970528     C* CALCOLA LA DATA CORRENTE
037500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
037600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
037700970528     C                   Z-ADD     WN8           G08DAT
037800970528     C                   Z-ADD     *ZEROS        G08INV
037900970528     C                   MOVEL     '0'           G08ERR
038000970528     C                   CALL      'XSRDA8'
038100970528     C                   PARM                    WLBDA8
038200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038300000310     C*
038400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
038500000310     C                   EXSR      AZZVID
038600970526     C*
038700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
038800970526     C                   EXSR      IMPFUN
038900970526     C*
039000970526     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
039300970526     C*------------------------------------------------------------------------*
039400970526     C     FUNGES        BEGSR
039500970526     C*
039600970526     C* PULISCO CAMPI DI LAVORO
039700970526     C                   Z-ADD     0             W                 5 0
039800970526     C*
039900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
040000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
040100970526     C     1             DO        24            I
040200970526     C                   MOVEL     FUD(I)        FUDRIG
040300970526     C*
040400970526     C* PULIZIA FUNZIONI PRECEDENTI
040500970526     C                   MOVEL     *BLANKS       FUV(I)
040600970526     C*
040700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
040800970526     C* (SALTA F24)
040900970526     C     FUDATT        IFEQ      'S'
041000970526     C     FUDFUN        ANDNE     '24'
041100970526     C                   ADD       1             W
041200970526     C                   MOVEL     FUDDES        FUV(W)
041300970526     C                   END
041400970526     C                   END                                                    DO WHILE
041500970526     C*
041600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
041700970526     C* ALTRIMENTI DISATTIVO F24
041800970526     C     1             DO        24            I
041900970526     C                   MOVEL     FUD(I)        FUDRIG
042000970526     C*
042100970526     C     FUDFUN        IFEQ      '24'
042200970526     C     W             IFGT      6
042300970526     C                   MOVEL     'S'           FUDATT
042400970526     C                   ELSE
042500970526     C                   MOVEL     *BLANKS       FUDATT
042600970526     C                   END                                                    W GT 8
042700970526     C                   MOVEL     FUDRIG        FUD(I)
042800970526     C                   END
042900970526     C                   END                                                    DO WHILE
043000970526     C*
043100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
043200970526     C     W             DIV       6             I
043300970526     C                   MVR                     Z
043400970526     C*
043500970526     C     Z             IFNE      0
043600970526     C     I             ADD       1             Z
043700970526     C                   ELSE
043800970526     C                   Z-ADD     I             Z                 5 0
043900970526     C                   END
044000970526     C*
044100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
044200970526     C                   ADD       1             FUNGRU
044300970526     C     FUNGRU        IFGT      Z
044400970526     C     FUNGRU        ORLT      1
044500970526     C                   Z-ADD     1             FUNGRU
044600970526     C                   END
044700970526     C*
044800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
044900970526     C     FUNGRU        IFEQ      1
045000970526     C                   MOVEL     FUV(1)        FUN001
045100970526     C                   MOVEL     FUV(2)        FUN002
045200970526     C                   MOVEL     FUV(3)        FUN003
045300970526     C                   MOVEL     FUV(4)        FUN004
045400970526     C                   MOVEL     FUV(5)        FUN005
045500970526     C                   MOVEL     FUV(6)        FUN006
045600970526     C                   END
045700970526     C*
045800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
045900970526     C     FUNGRU        IFEQ      2
046000970526     C                   MOVEL     FUV(7)        FUN001
046100970526     C                   MOVEL     FUV(8)        FUN002
046200970526     C                   MOVEL     FUV(9)        FUN003
046300970526     C                   MOVEL     FUV(10)       FUN004
046400970526     C                   MOVEL     FUV(11)       FUN005
046500970526     C                   MOVEL     FUV(12)       FUN006
046600970526     C                   END
046700970526     C*
046800970526     C* TERZO GRUPPO DI 6 FUNZIONI
046900970526     C     FUNGRU        IFEQ      3
047000970526     C                   MOVEL     FUV(13)       FUN001
047100970526     C                   MOVEL     FUV(14)       FUN002
047200970526     C                   MOVEL     FUV(15)       FUN003
047300970526     C                   MOVEL     FUV(16)       FUN004
047400970526     C                   MOVEL     FUV(17)       FUN005
047500970526     C                   MOVEL     FUV(18)       FUN006
047600970526     C                   END
047700970526     C*
047800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
047900970526     C     FUNGRU        IFEQ      4
048000970526     C                   MOVEL     FUV(19)       FUN001
048100970526     C                   MOVEL     FUV(20)       FUN002
048200970526     C                   MOVEL     FUV(21)       FUN003
048300970526     C                   MOVEL     FUV(22)       FUN004
048400970526     C                   MOVEL     *BLANKS       FUN005
048500970526     C                   MOVEL     *BLANKS       FUN006
048600970526     C                   END
048700970526     C*
048800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
048900970526     C                   MOVEL     *BLANKS       FUN007
049000970526     C                   MOVEL     *BLANKS       FUN008
049100970526     C*
049200970526     C     1             DO        24            I
049300970526     C                   MOVEL     FUD(I)        FUDRIG
049400970526     C     FUDFUN        IFEQ      '24'
049500970526     C     FUDATT        ANDEQ     'S'
049600970526     C                   MOVEL     FUDDES        FUN008
049700970526     C                   END
049800970526     C                   END                                                    DO
049900970526     C*
050000970526     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
050300970526     C*------------------------------------------------------------------------*
050400970526     C     FUNCON        BEGSR
050500970526     C*
050600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
050700970526     C                   EXSR      FUNVAR
050800970526     C*
050900970526     C* PULISCO CAMPI DI LAVORO
051000970526     C                   Z-ADD     0             I                 5 0
051100970526     C                   Z-ADD     0             W                 5 0
051200970526     C*
051300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
051400970526     C     FUNKEY        IFNE      *BLANKS
051500970526     C     1             DO        24            I
051600970526     C                   MOVEL     FUD(I)        FUDRIG
051700970526     C*
051800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
051900970526     C     FUNKEY        IFEQ      FUDFUN
052000970526     C     FUDATT        ANDEQ     'S'
052100970526     C                   Z-ADD     I             W
052200970526     C                   END
052300970526     C                   END                                                    DO WHILE
052400970526     C*
052500970526     C* FUNZIONE NON ATTIVA (ERRORE)
052600970526     C     W             IFEQ      0
052700970526     C*
052800970526     C* EMISSIONE MESSAGGIO DI ERRORE
052900970526     C                   MOVEL     MSG(1)        DSMSMS
053000970526     C*
053100970526     C                   END                                                    W=0
053200970526     C                   END                                                    FUNKEY NE *BLANKS
053300970526     C*
053400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
053500970526     C     W             IFNE      0
053600970526     C     FUNKEY        OREQ      *BLANKS
053700970526     C     FUNKEY        IFEQ      *BLANKS
053800970526     C                   EXSR      ENTGES
053900970526     C                   END
054000970526     C     FUNKEY        IFEQ      '03'
054100970526     C                   EXSR      F03GES
054200970526     C                   END
054300970526     C     FUNKEY        IFEQ      '06'
054400970526     C                   EXSR      F06GES
054500970526     C                   END
054600121024     C     FUNKEY        IFEQ      '07'
054700121024     C                   EXSR      F07RIP
054800121024     C                   END
054900970526     C     FUNKEY        IFEQ      '12'
055000970526     C                   EXSR      F12GES
055100970526     C                   END
055200970526     C     FUNKEY        IFEQ      '24'
055300970526     C                   EXSR      F24GES
055400970526     C                   END
055500970526     C                   END
055600970526     C*
055700970526     C                   ENDSR
055800970526     C*------------------------------------------------------------------------*
055900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
056000970526     C*------------------------------------------------------------------------*
056100970526     C     FUNVAR        BEGSR
056200970526     C*
056300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
056400970526     C*
056500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
056600970526     C     KEY           IFEQ      ENTER
056700970526     C                   MOVEL     *BLANKS       FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F1
057000970526     C                   MOVEL     '01'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F2
057300970526     C                   MOVEL     '02'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F3
057600970526     C                   MOVEL     '03'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F4
057900970526     C                   MOVEL     '04'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F5
058200970526     C                   MOVEL     '05'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F6
058500970526     C                   MOVEL     '06'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F7
058800970526     C                   MOVEL     '07'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F8
059100970526     C                   MOVEL     '08'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F9
059400970526     C                   MOVEL     '09'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F10
059700970526     C                   MOVEL     '10'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F11
060000970526     C                   MOVEL     '11'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F12
060300970526     C                   MOVEL     '12'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F13
060600970526     C                   MOVEL     '13'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F14
060900970526     C                   MOVEL     '14'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F15
061200970526     C                   MOVEL     '15'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F16
061500970526     C                   MOVEL     '16'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F17
061800970526     C                   MOVEL     '17'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F18
062100970526     C                   MOVEL     '18'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F19
062400970526     C                   MOVEL     '19'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F20
062700970526     C                   MOVEL     '20'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F21
063000970526     C                   MOVEL     '21'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F22
063300970526     C                   MOVEL     '22'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F23
063600970526     C                   MOVEL     '23'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F24
063900970526     C                   MOVEL     '24'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      ROLLUP
064200970526     C                   MOVEL     'RU'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      HELP
064500970526     C                   MOVEL     'HP'          FUNKEY
064600970526     C                   END
064700970526     C                   ENDSR
064800970526     C*------------------------------------------------------------------------*
064900970526     C* NOTE COSTRUZIONE SCHIERE
065000970526     C*------------------------------------------------------------------------*
065100970526     C*
065200970526     C* -MSG-MESSAGGI
065300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
065400970526     C* (NON TOCCARE MSG,1 E MSG,2)
065500970526     C*
065600970526     C* -CMD-COMANDI
065700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
065800970526     C*
065900970526     C* -OPD-OPZIONI
066000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
066100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
066200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
066300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
067000970526     C*
067100970526     C* -FUD-FUNZIONI
067200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
067300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
068000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
068100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
068200970526     O*------------------------------------------------------------------------*
068300970526** MSG - MESSAGGI
068400120927Tasto funzionale non gestito                                                   1
068500121127                                                                               2
068600121127                                                                               3
068700121127                                                                               4
068800121127                                                                               5
068900121127                                                                               6
069000121127                                                                               7
069100121127                                                                               8
069200121127                                                                               9
069300121127                                                                               10
069400121127                                                                               11
069500121127                                                                               12
069600121127                                                                               13
069700121127                                                                               14
069800121127                                                                               15
069900120927                                                                               16
070000120927                                                                               17
070100120927                                                                               18
070200120927                                                                               19
070300120927                                                                               20
070400121127                                                                               21
070500120927                                                                               22
070600121127                                                                               23
070700121127                                                                               24
070800970526** FUD - FUNZIONI
070900970526                   -        (S/N)=N                        funz.01¬
071000970526                   -        (S/N)=N                        funz.02¬
071100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
071200970526                   -        (S/N)=N                        funz.04¬
071300970526                   -        (S/N)=N                        funz.05¬
071400991214F06=Conferma       -        (S/N)=S                        funz.06¬
071500121127                   -        (S/N)=N                        funz.07¬
071600970527                   -        (S/N)=N                        funz.08¬
071700970527                   -        (S/N)=N                        funz.09¬
071800970527                   -        (S/N)=N                        funz.10¬
071900970526                   -        (S/N)=N                        funz.11¬
072000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
072100970526                   -        (S/N)=N                        funz.13¬
072200970526                   -        (S/N)=N                        funz.14¬
072300970526                   -        (S/N)=N                        funz.15¬
072400970526                   -        (S/N)=N                        funz.16¬
072500970526                   -        (S/N)=N                        funz.17¬
072600970526                   -        (S/N)=N                        funz.18¬
072700970526                   -        (S/N)=N                        funz.19¬
072800970526                   -        (S/N)=N                        funz.20¬
072900970526                   -        (S/N)=N                        funz.21¬
073000970526                   -        (S/N)=N                        funz.22¬
073100970526                   -        (S/N)=N                        funz.23¬
073200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

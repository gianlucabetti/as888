000100000000     H*------------------------------------------------------------------------*
000200030109     H* SALDIFICAZIONE MOVIMENTI GABBIE/BANCALI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800030109     FTISIG7D   CF   E             WORKSTN INFDS(DEVDS1)
000900030109     F                                     SFILE(SIG7DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* COMANDI
001900000000     D*-------------------
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* DESCRIZIONI TASTI FUNZIONALI
002300000000     D*-------------------
002400000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002500000000     D*-------------------
002600000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002700000000     D*-------------------
002800000000     D FUV             S             19    DIM(24)
002900000914     D*------------------
003000000914     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000914     D*------------------
003200000914     D WLBDA8          DS                  INZ
003300000914     D  G08DAT                 1      8  0
003400000914     D  G08INV                 9     16  0
003500000914     D  G08ERR                17     17
003600000914     D  G08TGI                18     22  0
003700000000     D*------------------------------------------------------------------------*
003800000000     D* INPUT
003900000000     D*------------------------------------------------------------------------*
004000000000     D*-------------------
004100000000     D* RIDEFINIZIONE SCHIERA -FUD-
004200000000     D*-------------------
004300000000     D FUDDS           DS
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE1                 1      1
004600000000     D*                -FUNZIONE
004700000000     D  FUDFUN                 2      3
004800000000     D*                -DESCRIZIONE PARZIALE
004900000000     D  FUDDE2                 4     19
005000000000     D*                -FILLER
005100000000     D  FUDFI0                20     34
005200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005300000000     D  FUDATT                35     35
005400000000     D*                -FILLER
005500000000     D  FUDFI1                36     80
005600000000     D*                -DESCRIZIONE COMPLETA
005700000000     D  FUDDES                 1     19
005800000000     D*                -RIDEFINIZIONE RIGA
005900000000     D  FUDRIG                 1     80
006000000000     D*-------------------
006100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006200000000     D*-------------------
006300000000     D DSMSG         E DS
006400000000     D*-------------------
006500000000     D* DS PARAMETRI DI LANCIO PGM
006600000000     D*-------------------
006700950703     D PARAM           DS
006800071217     D  PARDAF                        8  0
006900071217     D  PARDLT                        1
007000000000     D*-------------------
007100000000     D* DS PER GESTIONE TASTI FUNZIONALI
007200000000     D*-------------------
007300000000     D DSKEY         E DS
007400000000     D DEVDS1          DS
007500000000     D  KEY                  369    369
007600030123     D*------------
007700030123     D* DS REPERIMENTO DATI UTENTE
007800030123     D*------------
007900030123     D TIBS34DS      E DS                                                       *Profili utente
008000030123     D DDATIUTE      E DS                                                       *Dati utente
008100030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008200000000     D*-------------------
008300000000     D* ARCHITETTURA
008400000000     D*-------------------
008500000000     D KPJBA         E DS
008600071217     D*-------------------
008700071217     D* VARIABILI D WRK
008800071217     D*-------------------
008900071217     D DataIso         s               d   datfmt(*iso)
009000071217     D DataLimiteMin   s               d   datfmt(*iso)
009100000000     C*------------------------------------------------------------------------*
009200000000     C* MAIN LINE
009300000000     C*------------------------------------------------------------------------*
009400000000     C*
009500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009700000000     C*
009800000000     C* CICLO FINO A CHE:
009900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
010000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
010100000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
010200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
010300000000     C     F12L00        OREQ      '1'
010400000000     C     FLFINE        OREQ      '1'
010500000000     C*
010600000000     C* EMETTE FORMATO VIDEO
010700000000     C                   EXSR      BUTFMR
010800000000     C*
010900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
011000000000     C                   EXSR      FUNCON
011100000000     C*
011200000914     C                   ENDDO                                                  <-- 2 --
011300000000     C*
011400000914     C                   ENDIF                                                  <-- 1 --
011500991118     C*
011600000000     C                   SETON                                        LR
011700000000     C*------------------------------------------------------------------------*
011800000000     C* EMISSIONE FORMATO A VIDEO
011900000000     C*------------------------------------------------------------------------*
012000000000     C     BUTFMR        BEGSR
012100000000     C*
012200000000     C* WRITE RECORD TESTATA
012300030109     C                   WRITE     SIG7D01
012400000000     C*
012500000000     C* VISUALIZZAZIONE MESSAGGIO
012600000000     C                   EXSR      SNDMSG
012700000000     C*
012800000000     C* WRITE RECORD FUNZIONI
012900030109     C                   WRITE     SIG7D02
013000991019     C*
013100991019     C* EXFMT RECORD DETTAGLIO
013200030109     C                   EXFMT     SIG7D03
013300991019     C*
013400000000     C* AZZERO FLAG DI ERRORE LIVELLO
013500000000     C                   MOVEL     '0'           ERRL00
013600000000     C*
013700000000     C* AZZERO FLAG DI ERRORE
013800991025     C                   SETOFF                                         7071
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*------------------------------------------------------------------------*
014200000000     C* SNDMSG - MANDA MESSAGGIO
014300000000     C*------------------------------------------------------------------------*
014400000000     C     SNDMSG        BEGSR
014500000000     C*
014600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014700000000     C                   MOVEL     '1'           *IN40
014800030109     C                   WRITE     SIG7DMC
014900000000     C                   MOVEL     '0'           *IN40
015000000000     C*
015100000000     C* VISUALIZZAZIONE MESSAGGIO
015200000000     C                   Z-ADD     0             MSGULT            4 0
015300000000     C                   CALL      'XMSG'        XMSGPA
015400000000     C     DSMSER        IFEQ      0
015500000000     C                   MOVEL     DSMSKE        MSGKEY
015600000000     C                   END
015700000000     C                   Z-ADD     1             MSGULT
015800030109     C                   WRITE     SIG7DMS                                      SFL MESS
015900030109     C                   WRITE     SIG7DMC                                      CONTR MESS
016000000000     C*
016100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016200000000     C                   MOVEL     *BLANKS       DSMSMS
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016700000000     C*------------------------------------------------------------------------*
016800000000     C     ENTGES        BEGSR
016900000000     C*
017000000000     C* CONTROLLA I CAMPI IMMESSI
017100000000     C                   EXSR      DECODI
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017600000000     C*------------------------------------------------------------------------*
017700000000     C     F03GES        BEGSR
017800000000     C*
017900000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
018000000000     C                   MOVEL     '1'           F03L00
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018500000000     C*------------------------------------------------------------------------*
018600991020     C     F06GES        BEGSR
018700000000     C*
018800991105     C                   EXSR      DECODI
018900991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
019000991105     C     ERRL00        IFEQ      '0'
019100991105     C*
019200000914     C* LANCIO IL PROGRAMMA DI GENERAZIONE ARCHIVIO
019300030109     C                   MOVEL     'SIG8'        KCOAZ
019400000000     C                   EXSR      VIDPAR
019500950703     C                   MOVEL     PARAM         KPJBU
019600071217     C                   CALL      'BCH10'
019700071217 MOM C***                CALL      'TISIG8C'
019800030109     C                   PARM                    KPJBA
019900000927     C*
020000000927     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020100000927     C                   MOVEL     '1'           F03L00
020200000914     C*
020300991105     C                   ENDIF
020400991105     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
020800000000     C*------------------------------------------------------------------------*
020900000000     C     F12GES        BEGSR
021000000000     C*
021100000000     C* IMPOSTO FLAG DI LIVELLO
021200000000     C                   MOVEL     '1'           F12L00
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
021700000000     C*------------------------------------------------------------------------*
021800000000     C     F24GES        BEGSR
021900000000     C*
022000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
022100000000     C                   EXSR      FUNGES
022200000000     C*
022300000000     C                   ENDSR
022400000914     C*------------------------------------------------------------------------*
022500000914     C* CHKDA8 - CONTROLLA LA DATA (8)
022600000914     C*------------------------------------------------------------------------*
022700000914     C     CHKDA8        BEGSR
022800030109     C*
022900030109     C     *LIKE         DEFINE    VIDDAF        WDA8                           gg/mm/aaaa
023000030109     C     *LIKE         DEFINE    VIDDAF        DA8W                           aaaa/mm/gg
023100000914     C*
023200030109     C                   Z-ADD     VIDDAF        G08DAT
023300000914     C                   Z-ADD     *ZEROS        G08INV
023400000914     C                   MOVEL     '0'           G08ERR
023500000914     C                   CALL      'XSRDA8'
023600000914     C                   PARM                    WLBDA8
023700030109     C                   Z-ADD     G08DAT        WDA8
023800030109     C                   Z-ADD     G08INV        DA8W
023900030109     C     G08ERR        IFEQ      '1'
024000000914     C                   MOVEL     '1'           ERRL00
024100030109     C                   ENDIF
024200000914     C*
024300000914     C                   ENDSR
024400991020     C*------------------------------------------------------------------------*
024500000914     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024600000000     C*------------------------------------------------------------------------*
024700000000     C     DECODI        BEGSR
024800000914     C*
024900000914     C                   MOVEL     '0'           ERRL00
025000000914     C*--------------------
025100000914     C* DATA FINALE
025200000914     C*--------------------
025300030109     C     *LIKE         DEFINE    VIDDAF        DAFVID
025400030109     C     ERRL00        IFEQ      '0'
025500000914     C                   Z-ADD     VIDDAF        WDA8
025600000914     C                   Z-ADD     *ZEROS        DA8W
025700000914     C                   EXSR      CHKDA8
025800000914     C                   Z-ADD     WDA8          VIDDAF
025900030109     C                   Z-ADD     DA8W          DAFVID
026000030109     C     ERRL00        IFEQ      '1'
026100000914     C                   SETON                                        71
026200000914     C                   MOVEL     MSG(16)       DSMSMS
026300030109     C                   ENDIF
026400030109     C                   ENDIF
026500071217     C*
026600071217     C*--------------------
026700071217     C* CONTROLLO DATA FINALE ALMENO 365 GIORNI ADDIETRO DATA CORRENTE
026800071217     C*--------------------
026900071217     C                   IF        ERRL00 = '0'
027000071217     C                   time                    TimeCorr         14 0
027100071217     C                   move(p)   TimeCorr      DataCorr          8 0
027200071217     C     *eur          movel     DataCorr      DataIso
027300071217     C     DataIso       subdur    365:*d        DataLimiteMin
027400071217     C     *iso          movel     DAFVID        DataIso
027500071217     C                   if        DataIso > DataLimiteMin
027600071217     C                   SETON                                        71
027700071217     C                   MOVEL     MSG(8)        DSMSMS
027800071217     C                   MOVEL     '1'           ERRL00
027900071217     C                   endif
028000071217     C                   ENDIF
028100000000     C*
028200000000     C                   ENDSR
028300000000     C*------------------------------------------------------------------------*
028400000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
028500000000     C*------------------------------------------------------------------------*
028600000000     C     AZZVID        BEGSR
028700000000     C*
028800030109     C                   Z-ADD     *ZEROS        VIDDAF
028900071217     C                   MOVEL     'N'           VIDDLT
029000000000     C*
029100000000     C                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
029400000000     C*------------------------------------------------------------------------*
029500000000     C     VIDPAR        BEGSR
029600000000     C*
029700000914     C                   Z-ADD     DAFVID        PARDAF
029800071217     C                   MOVEL     VIDDLT        PARDLT
029900000000     C*
030000000000     C                   ENDSR
030100030123     C*--------------------------------------------------------------------------------------------*
030200030123     C* REPERISCE I DATI UTENTE
030300030123     C*--------------------------------------------------------------------------------------------*
030400030123     C     REPDATIUTE    BEGSR
030500030123     C*
030600030123     C* INIZIALIZZA VARIABILI DI WRK
030700030123     C                   CLEAR                   TIBS34DS
030800030123     C                   CLEAR                   AZUTEDS
030900030123     C                   CLEAR                   DDATIUTE
031000030123     C*
031100030123     C     *DTAARA       DEFINE    §azute        azuteds
031200030123     C     *DTAARA       DEFINE    §datiute      ddatiute
031300030123     C                   IN(E)     *DTAARA
031400030123     C                   IF        %Error
031500030123     C                   EVAL      I34Tla = 'L'
031600030123     C                   CALL      'TIBS34R'
031700030123     C                   PARM                    Tibs34Ds
031800030123     C                   IN        *DTAARA
031900030123     C                   ENDIF
032000030123     C*
032100030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
032200030123     C                   MOVEL(P)  RSUT          DSFIRS           20
032300030123     C*
032400030123     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600980403     C* *INZSR - OPERAZIONI INIZIALI
032700000000     C*------------------------------------------------------------------------*
032800980403     C     *INZSR        BEGSR
032900000000     C*
033000000000     C* RICEVIMENTO PARAMETRI
033100000000     C     *ENTRY        PLIST
033200000000     C                   PARM                    KPJBA
033300030123     C*
033400030123     C* REPERIMENTO DATI UTENTE
033500030123     C                   EXSR      REPDATIUTE
033600000000     C*
033700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
033800000000     C     XMSGPA        PLIST
033900000000     C                   PARM                    DSMSMS
034000000000     C                   PARM                    DSMSPG
034100000000     C                   PARM                    DSMSKE
034200000000     C                   PARM                    DSMSER
034300000000     C*
034400000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
034500000000     C                   CALL      'XKEY'
034600000000     C                   PARM                    DSKEY
034700000000     C*
034800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
034900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
035000000000     C*
035100000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
035200000000     C*   -NOME PGM CHIAMANTE
035300000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
035400030109     C                   MOVEL     'TISIG7R'     DSMSPG
035500030109     C                   MOVEL     'TISIG7R'     MSGQUE
035600000000     C                   MOVEL     *BLANKS       MSGKEY
035700000000     C                   MOVEL     *BLANKS       DSMSMS
035800000000     C*
035900000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
036000000000     C                   Z-ADD     0             FUNGRU            5 0
036100000000     C*
036200000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
036300000000     C                   EXSR      FUNGES
036400000000     C*
036500010208     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
036600000000     C                   MOVEL     '0'           F12L00            1
036700000000     C                   MOVEL     '0'           F03L00            1
036800000000     C                   MOVEL     '0'           FINL00            1
036900000000     C                   MOVEL     '0'           ERRL00            1
037000000000     C                   MOVEL     '0'           FLFINE            1
037100000000     C*
037200000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
037300000000     C                   EXSR      AZZVID
037400010208     C*
037500000000     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
037800000000     C*------------------------------------------------------------------------*
037900000000     C     FUNGES        BEGSR
038000000000     C*
038100000000     C* PULISCO CAMPI DI LAVORO
038200000000     C                   Z-ADD     0             W                 5 0
038300000000     C*
038400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
038500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
038600000000     C     1             DO        24            I
038700000000     C                   MOVEL     FUD(I)        FUDRIG
038800000000     C*
038900000000     C* PULIZIA FUNZIONI PRECEDENTI
039000000000     C                   MOVEL     *BLANKS       FUV(I)
039100000000     C*
039200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
039300000000     C* (SALTA F24)
039400000000     C     FUDATT        IFEQ      'S'
039500000000     C     FUDFUN        ANDNE     '24'
039600000000     C                   ADD       1             W
039700000000     C                   MOVEL     FUDDES        FUV(W)
039800000000     C                   END
039900000000     C                   END                                                    DO WHILE
040000000000     C*
040100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
040200000000     C* ALTRIMENTI DISATTIVO F24
040300000000     C     1             DO        24            I
040400000000     C                   MOVEL     FUD(I)        FUDRIG
040500000000     C*
040600000000     C     FUDFUN        IFEQ      '24'
040700000000     C     W             IFGT      6
040800000000     C                   MOVEL     'S'           FUDATT
040900000000     C                   ELSE
041000000000     C                   MOVEL     *BLANKS       FUDATT
041100000000     C                   END                                                    W GT 8
041200000000     C                   MOVEL     FUDRIG        FUD(I)
041300000000     C                   END
041400000000     C                   END                                                    DO WHILE
041500000000     C*
041600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
041700000000     C     W             DIV       6             I
041800000000     C                   MVR                     Z
041900000000     C*
042000000000     C     Z             IFNE      0
042100000000     C     I             ADD       1             Z
042200000000     C                   ELSE
042300000000     C                   Z-ADD     I             Z                 5 0
042400000000     C                   END
042500000000     C*
042600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
042700000000     C                   ADD       1             FUNGRU
042800000000     C     FUNGRU        IFGT      Z
042900000000     C     FUNGRU        ORLT      1
043000000000     C                   Z-ADD     1             FUNGRU
043100000000     C                   END
043200000000     C*
043300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
043400000000     C     FUNGRU        IFEQ      1
043500000000     C                   MOVEL     FUV(1)        FUN001
043600000000     C                   MOVEL     FUV(2)        FUN002
043700000000     C                   MOVEL     FUV(3)        FUN003
043800000000     C                   MOVEL     FUV(4)        FUN004
043900000000     C                   MOVEL     FUV(5)        FUN005
044000000000     C                   MOVEL     FUV(6)        FUN006
044100000000     C                   END
044200000000     C*
044300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
044400000000     C     FUNGRU        IFEQ      2
044500000000     C                   MOVEL     FUV(7)        FUN001
044600000000     C                   MOVEL     FUV(8)        FUN002
044700000000     C                   MOVEL     FUV(9)        FUN003
044800000000     C                   MOVEL     FUV(10)       FUN004
044900000000     C                   MOVEL     FUV(11)       FUN005
045000000000     C                   MOVEL     FUV(12)       FUN006
045100000000     C                   END
045200000000     C*
045300000000     C* TERZO GRUPPO DI 6 FUNZIONI
045400000000     C     FUNGRU        IFEQ      3
045500000000     C                   MOVEL     FUV(13)       FUN001
045600000000     C                   MOVEL     FUV(14)       FUN002
045700000000     C                   MOVEL     FUV(15)       FUN003
045800000000     C                   MOVEL     FUV(16)       FUN004
045900000000     C                   MOVEL     FUV(17)       FUN005
046000000000     C                   MOVEL     FUV(18)       FUN006
046100000000     C                   END
046200000000     C*
046300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
046400000000     C     FUNGRU        IFEQ      4
046500000000     C                   MOVEL     FUV(19)       FUN001
046600000000     C                   MOVEL     FUV(20)       FUN002
046700000000     C                   MOVEL     FUV(21)       FUN003
046800000000     C                   MOVEL     FUV(22)       FUN004
046900000000     C                   MOVEL     *BLANKS       FUN005
047000000000     C                   MOVEL     *BLANKS       FUN006
047100000000     C                   END
047200000000     C*
047300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
047400000000     C                   MOVEL     *BLANKS       FUN007
047500000000     C                   MOVEL     *BLANKS       FUN008
047600000000     C*
047700000000     C     1             DO        24            I
047800000000     C                   MOVEL     FUD(I)        FUDRIG
047900000000     C     FUDFUN        IFEQ      '24'
048000000000     C     FUDATT        ANDEQ     'S'
048100000000     C                   MOVEL     FUDDES        FUN008
048200000000     C                   END
048300000000     C                   END                                                    DO
048400000000     C                   ENDSR
048500000000     C*------------------------------------------------------------------------*
048600000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
048700000000     C*------------------------------------------------------------------------*
048800000000     C     FUNCON        BEGSR
048900000000     C*
049000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
049100000000     C                   EXSR      FUNVAR
049200000000     C*
049300000000     C* PULISCO CAMPI DI LAVORO
049400000000     C                   Z-ADD     0             I                 5 0
049500000000     C                   Z-ADD     0             W                 5 0
049600000000     C*
049700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
049800000000     C     FUNKEY        IFNE      *BLANKS
049900000000     C     1             DO        24            I
050000000000     C                   MOVEL     FUD(I)        FUDRIG
050100000000     C*
050200000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
050300000000     C     FUNKEY        IFEQ      FUDFUN
050400000000     C     FUDATT        ANDEQ     'S'
050500000000     C                   Z-ADD     I             W
050600000000     C                   END
050700000000     C                   END                                                    DO WHILE
050800000000     C*
050900000000     C* FUNZIONE NON ATTIVA (ERRORE)
051000000000     C     W             IFEQ      0
051100000000     C*
051200000000     C* EMISSIONE MESSAGGIO DI ERRORE
051300000000     C                   MOVEL     MSG(1)        DSMSMS
051400000000     C*
051500000000     C                   END                                                    W=0
051600000000     C                   END                                                    FUNKEY NE *BLANKS
051700000000     C*
051800000000     C* FUNZIONE ATTIVA (PROCEDI ...)
051900000000     C     W             IFNE      0
052000000000     C     FUNKEY        OREQ      *BLANKS
052100000000     C*
052200000000     C     FUNKEY        IFEQ      *BLANKS
052300000000     C                   EXSR      ENTGES
052400000000     C                   END
052500000000     C     FUNKEY        IFEQ      '03'
052600000000     C                   EXSR      F03GES
052700000000     C                   END
052800000000     C     FUNKEY        IFEQ      '06'
052900000000     C                   EXSR      F06GES
053000000000     C                   END
053100000000     C     FUNKEY        IFEQ      '12'
053200000000     C                   EXSR      F12GES
053300000000     C                   END
053400000000     C     FUNKEY        IFEQ      '24'
053500000000     C                   EXSR      F24GES
053600000000     C                   END
053700000000     C*
053800000000     C                   END
053900000000     C*
054000000000     C                   ENDSR
054100000000     C*------------------------------------------------------------------------*
054200000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
054300000000     C*------------------------------------------------------------------------*
054400000000     C     FUNVAR        BEGSR
054500000000     C*
054600000000     C* INIZIALIZZA VARIABILE -FUNKEY-
054700000000     C*
054800000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
054900000000     C     KEY           IFEQ      ENTER
055000000000     C                   MOVEL     *BLANKS       FUNKEY
055100000000     C                   END
055200000000     C     KEY           IFEQ      F1
055300000000     C                   MOVEL     '01'          FUNKEY
055400000000     C                   END
055500000000     C     KEY           IFEQ      F2
055600000000     C                   MOVEL     '02'          FUNKEY
055700000000     C                   END
055800000000     C     KEY           IFEQ      F3
055900000000     C                   MOVEL     '03'          FUNKEY
056000000000     C                   END
056100000000     C     KEY           IFEQ      F4
056200000000     C                   MOVEL     '04'          FUNKEY
056300000000     C                   END
056400000000     C     KEY           IFEQ      F5
056500000000     C                   MOVEL     '05'          FUNKEY
056600000000     C                   END
056700000000     C     KEY           IFEQ      F6
056800000000     C                   MOVEL     '06'          FUNKEY
056900000000     C                   END
057000000000     C     KEY           IFEQ      F7
057100000000     C                   MOVEL     '07'          FUNKEY
057200000000     C                   END
057300000000     C     KEY           IFEQ      F8
057400000000     C                   MOVEL     '08'          FUNKEY
057500000000     C                   END
057600000000     C     KEY           IFEQ      F9
057700000000     C                   MOVEL     '09'          FUNKEY
057800000000     C                   END
057900000000     C     KEY           IFEQ      F10
058000000000     C                   MOVEL     '10'          FUNKEY
058100000000     C                   END
058200000000     C     KEY           IFEQ      F11
058300000000     C                   MOVEL     '11'          FUNKEY
058400000000     C                   END
058500000000     C     KEY           IFEQ      F12
058600000000     C                   MOVEL     '12'          FUNKEY
058700000000     C                   END
058800000000     C     KEY           IFEQ      F13
058900000000     C                   MOVEL     '13'          FUNKEY
059000000000     C                   END
059100000000     C     KEY           IFEQ      F14
059200000000     C                   MOVEL     '14'          FUNKEY
059300000000     C                   END
059400000000     C     KEY           IFEQ      F15
059500000000     C                   MOVEL     '15'          FUNKEY
059600000000     C                   END
059700000000     C     KEY           IFEQ      F16
059800000000     C                   MOVEL     '16'          FUNKEY
059900000000     C                   END
060000000000     C     KEY           IFEQ      F17
060100000000     C                   MOVEL     '17'          FUNKEY
060200000000     C                   END
060300000000     C     KEY           IFEQ      F18
060400000000     C                   MOVEL     '18'          FUNKEY
060500000000     C                   END
060600000000     C     KEY           IFEQ      F19
060700000000     C                   MOVEL     '19'          FUNKEY
060800000000     C                   END
060900000000     C     KEY           IFEQ      F20
061000000000     C                   MOVEL     '20'          FUNKEY
061100000000     C                   END
061200000000     C     KEY           IFEQ      F21
061300000000     C                   MOVEL     '21'          FUNKEY
061400000000     C                   END
061500000000     C     KEY           IFEQ      F22
061600000000     C                   MOVEL     '22'          FUNKEY
061700000000     C                   END
061800000000     C     KEY           IFEQ      F23
061900000000     C                   MOVEL     '23'          FUNKEY
062000000000     C                   END
062100000000     C     KEY           IFEQ      F24
062200000000     C                   MOVEL     '24'          FUNKEY
062300000000     C                   END
062400000000     C     KEY           IFEQ      ROLLUP
062500000000     C                   MOVEL     'RU'          FUNKEY
062600000000     C                   END
062700000000     C     KEY           IFEQ      HELP
062800000000     C                   MOVEL     'HP'          FUNKEY
062900000000     C                   END
063000000000     C                   ENDSR
063100000000     C*------------------------------------------------------------------------*
063200000000     C* NOTE PER COMPILAZIONE SCHIERE
063300000000     C*------------------------------------------------------------------------*
063400000000     C* -MSG-
063500000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
063600000000     C* (NON TOCCARE MSG,1)
063700000000     C*
063800000000     C* -CMD-
063900000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
064000000000     C*
064100000000     C* -FUD-
064200000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
064300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
064400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
064500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
064600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
064700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
064800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
064900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
065000000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
065100000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
065200000000     C*
065300000000     C*------------------------------------------------------------------------*
065400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
065500030109TISIG7R- Tasto funzionale non gestito.                                         1
065600030109TISIG7R-
065700030109TISIG7R- Data non accettabile.                                                 3
065800030109TISIG7R-
065900030109TISIG7R-
066000030109TISIG7R- Data inizio maggiore data fine.                                       6
066100030109TISIG7R- Codice CONSEGNA ANOMALE non valido per POSTE.                         7
066200071217TISIG7R- Data troppo recente: devono essere trascorsi almeno 365 giorni        8
066300030109TISIG7R-
066400030109TISIG7R-
066500030109TISIG7R-
066600030109TISIG7R-
066700030109TISIG7R-
066800030109TISIG7R-
066900030109TISIG7R-
067000030109TISIG7R- Data non valida.                                                     16
067100030109TISIG7R-
067200030109TISIG7R-
067300030109TISIG7R-
067400030109TISIG7R-
067500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
067600000000WRKSPLF
067700000000
067800000000
067900000000
068000000000
068100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
068200000000                   -        (S/N)=N                        funz.01¬
068300000000                   -        (S/N)=N                        funz.02¬
068400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
068500000000                   -        (S/N)=N                        funz.04¬
068600000000                   -        (S/N)=N                        funz.05¬
068700000000F06=Lancio         -        (S/N)=S                        funz.06¬
068800000000                   -        (S/N)=N                        funz.07¬
068900000000                   -        (S/N)=N                        funz.08¬
069000000000                   -        (S/N)=N                        funz.09¬
069100000000                   -        (S/N)=N                        funz.10¬
069200000000                   -        (S/N)=N                        funz.11¬
069300000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
069400000000                   -        (S/N)=N                        funz.13¬
069500000000                   -        (S/N)=N                        funz.14¬
069600000000                   -        (S/N)=N                        funz.15¬
069700000000                   -        (S/N)=N                        funz.16¬
069800000000                   -        (S/N)=N                        funz.17¬
069900000000                   -        (S/N)=N                        funz.18¬
070000000000                   -        (S/N)=N                        funz.19¬
070100000000                   -        (S/N)=N                        funz.20¬
070200000000                   -        (S/N)=N                        funz.21¬
070300000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
070400000000                   -        (S/N)=N                        funz.23¬
070500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬

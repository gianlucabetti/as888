000100000000     H*------------------------------------------------------------------------*
000200060520     H* CALCOLO INSTRADAMENTO CAPPARIO DPD GeoPost - TEST
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800060520     FTISIE4D   CF   E             WORKSTN INFDS(DEVDS1)
000900060520     F                                     SFILE(SIE4DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* DESCRIZIONI TASTI FUNZIONALI
001900000000     D*-------------------
002000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002300000000     D*-------------------
002400000000     D FUV             S             19    DIM(24)
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* RIDEFINIZIONE SCHIERA -FUD-
003000000000     D*-------------------
003100000000     D FUDDS           DS
003200000000     D*                -DESCRIZIONE PARZIALE
003300000000     D  FUDDE1                 1      1
003400000000     D*                -FUNZIONE
003500000000     D  FUDFUN                 2      3
003600000000     D*                -DESCRIZIONE PARZIALE
003700000000     D  FUDDE2                 4     19
003800000000     D*                -FILLER
003900000000     D  FUDFI0                20     34
004000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004100000000     D  FUDATT                35     35
004200000000     D*                -FILLER
004300000000     D  FUDFI1                36     80
004400000000     D*                -DESCRIZIONE COMPLETA
004500000000     D  FUDDES                 1     19
004600000000     D*                -RIDEFINIZIONE RIGA
004700000000     D  FUDRIG                 1     80
004800000000     D*-------------------
004900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005000000000     D*-------------------
005100000000     D DSMSG         E DS
005200000000     D*-------------------
005300020919     D* DS REPERIMENTO DATI UTENTE
005400020919     D*------------
005500020919     D TIBS34DS      E DS                                                       *Profili utente
005600020919     D DDATIUTE      E DS                                                       *Dati utente
005700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005800060520     D*--------------------
005900060520     D* DS "XSRDAT" - CONTROLLO DATA (8)
006000060520     D*--------------------
006100060520     DWLBDA8           DS                  INZ
006200060520     D  G08DAT                 1      8  0
006300060520     D  G08INV                 9     16  0
006400060520     D  G08ERR                17     17
006500060520     D  G08TGI                18     22  0
006600000104     D*--------------------
006700060520     D* DS D PROCEDURA
006800000104     D*--------------------
006900060520     D TISIE3DS      E DS                  INZ
007000000000     D*-------------------
007100000000     D* DS PER GESTIONE TASTI FUNZIONALI
007200000000     D*-------------------
007300000000     D DSKEY         E DS
007400000000     D DEVDS1          DS
007500000000     D  KEY                  369    369
007600000000     D*-------------------
007700000000     D* ARCHITETTURA
007800000000     D*-------------------
007900000000     D KPJBA         E DS
008000060721     D*-------------------
008100060721     D* VARIABILI D WRK
008200060721     D*-------------------
008300060721     D wTimeStr        s               z   inz
008400060721     D wTimeEnd        s               z   inz
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800000000     C*
008900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009100000000     C*
009200000000     C* CICLO FINO A CHE:
009300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009500000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009700000000     C     F12L00        OREQ      '1'
009800000000     C     FLFINE        OREQ      '1'
009900000000     C*
010000000000     C* EMETTE FORMATO VIDEO
010100000000     C                   EXSR      BUTFMR
010200000000     C*
010300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010400000000     C                   EXSR      FUNCON
010500000000     C*
010600000000     C                   END                                                    <-- 2 --
010700000000     C*
010800000000     C                   END                                                    <-- 1 --
010900060608     C*
011000060608     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD SOLO X CHIUSURA
011100060608     C                   CLEAR                   TISIE3DS
011200060608     C                   EVAL      ISIE3TLA = 'C'
011300060608     C                   CALL      'TISIE3R'
011400060608     C                   PARM                    TISIE3DS
011500000000     C*
011600000000     C                   SETON                                        LR
011700000000     C*------------------------------------------------------------------------*
011800000000     C* EMISSIONE FORMATO A VIDEO
011900000000     C*------------------------------------------------------------------------*
012000000000     C     BUTFMR        BEGSR
012100000000     C*
012200000000     C* WRITE RECORD TESTATA
012300060520     C                   WRITE     SIE4D01
012400000000     C*
012500000000     C* VISUALIZZAZIONE MESSAGGIO
012600000000     C                   EXSR      SNDMSG
012700000000     C*
012800000000     C* WRITE RECORD FUNZIONI
012900060520     C                   WRITE     SIE4D02
013000991019     C*
013100991019     C* EXFMT RECORD DETTAGLIO
013200060520     C                   EXFMT     SIE4D03
013300991019     C*
013400000000     C* AZZERO FLAG DI ERRORE LIVELLO
013500000000     C                   MOVEL     '0'           ERRL00
013600000000     C*
013700000000     C* AZZERO FLAG DI ERRORE
013800060520     C                   SETOFF                                       70
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*------------------------------------------------------------------------*
014200000000     C* SNDMSG - MANDA MESSAGGIO
014300000000     C*------------------------------------------------------------------------*
014400000000     C     SNDMSG        BEGSR
014500000000     C*
014600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014700000000     C                   MOVEL     '1'           *IN40
014800060520     C                   WRITE     SIE4DMC
014900000000     C                   MOVEL     '0'           *IN40
015000000000     C*
015100000000     C* VISUALIZZAZIONE MESSAGGIO
015200000000     C                   Z-ADD     0             MSGULT            4 0
015300000000     C                   CALL      'XMSG'        XMSGPA
015400000000     C     DSMSER        IFEQ      0
015500000000     C                   MOVEL     DSMSKE        MSGKEY
015600000000     C                   END
015700000000     C                   Z-ADD     1             MSGULT
015800060520     C                   WRITE     SIE4DMS                                      SFL MESS
015900060520     C                   WRITE     SIE4DMC                                      CONTR MESS
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
017100060520     C                   EXSR      DECODI
017200060520     C*
017300000216     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
017400000216     C     ERRL00        IFEQ      '0'
017500060721     C*
017600060721     C* Salvo il timestamp prima d eseguire la ricerca
017700060721     C                   TIME                    wTimeStr
017800000216     C*
017900000216     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
018000060608     C                   EVAL      ISIE3TLA = 'E'
018100060520     C                   CALL      'TISIE3R'
018200060520     C                   PARM                    TISIE3DS
018300060721     C*
018400060721     C* Salvo il timestamp dopo aver eseguito la ricerca
018500060721     C                   TIME                    wTimeEnd
018600060520     C*
018700060520     C* Se errore evidenzio bene a video
018800060520     C                   IF        OSIE3ERR <> *blanks
018900060520     C                   SETON                                        70
019000060520     C                   ENDIF
019100000216     C                   ENDIF
019200060721     C*
019300060721     C* Se richiesto debug performance visualizzo i risultati
019400060721     C                   IF        VIDDBG = 'S'
019500060721     C     wTimeStr      DSPLY
019600060721     C     wTimeEnd      DSPLY
019700060721     C                   ENDIF
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
020200000000     C*------------------------------------------------------------------------*
020300000000     C     F03GES        BEGSR
020400000000     C*
020500000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020600000000     C                   MOVEL     '1'           F03L00
020700000000     C*
020800000000     C                   ENDSR
020900060520     C*------------------------------------------------------------------------*
021000060520     C* F05GES -GESTIONE TASTO FUNZIONALE F05 - INIZIALIZZA VIDEATA
021100060520     C*------------------------------------------------------------------------*
021200060520     C     F05GES        BEGSR
021300060520     C*
021400060520     C                   CLEAR                   TISIE3DS
021500060520     C                   EVAL      ISIE3EXC = 'N'
021600060520     C                   Z-ADD     DATCOR        ISIE3DRI
021700060721     C                   EVAL      VIDDBG = 'N'
021800060520     C*
021900060520     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
022200000000     C*------------------------------------------------------------------------*
022300000000     C     F12GES        BEGSR
022400000000     C*
022500000000     C* IMPOSTO FLAG DI LIVELLO
022600991103     C                   MOVEL     '1'           F12L00
022700000000     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023100000000     C*------------------------------------------------------------------------*
023200000000     C     F24GES        BEGSR
023300000000     C*
023400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
023500000000     C                   EXSR      FUNGES
023600000000     C*
023700000000     C                   ENDSR
023800991020     C*------------------------------------------------------------------------*
023900000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024000000000     C*------------------------------------------------------------------------*
024100000000     C     DECODI        BEGSR
024200000216     C*
024300060520     C                   SETOFF                                       70
024400000216     C                   MOVEL     '0'           ERRL00
024500000000     C*
024600000000     C                   ENDSR
024700020919     C*--------------------------------------------------------------------------------------------*
024800020919     C* REPERISCE I DATI UTENTE
024900020919     C*--------------------------------------------------------------------------------------------*
025000020919     C     REPDATIUTE    BEGSR
025100020919     C*
025200020919     C* INIZIALIZZA VARIABILI DI WRK
025300020919     C                   CLEAR                   TIBS34DS
025400020919     C                   CLEAR                   AZUTEDS
025500020919     C                   CLEAR                   DDATIUTE
025600020919     C*
025700020919     C     *DTAARA       DEFINE    §azute        azuteds
025800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
025900020919     C                   IN(E)     *DTAARA
026000060520     C                   IF        %Error
026100060520     C                   EVAL      I34Tla = 'L'
026200020919     C                   CALL      'TIBS34R'
026300020919     C                   PARM                    Tibs34Ds
026400020919     C                   IN        *DTAARA
026500060520     C                   ENDIF
026600020919     C*
026700020919     C                   ENDSR
026800000000     C*------------------------------------------------------------------------*
026900980403     C* *INZSR - OPERAZIONI INIZIALI
027000000000     C*------------------------------------------------------------------------*
027100980403     C     *INZSR        BEGSR
027200000000     C*
027300000000     C* RICEVIMENTO PARAMETRI
027400000000     C     *ENTRY        PLIST
027500000000     C                   PARM                    KPJBA
027600000000     C*
027700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
027800000000     C     XMSGPA        PLIST
027900000000     C                   PARM                    DSMSMS
028000000000     C                   PARM                    DSMSPG
028100000000     C                   PARM                    DSMSKE
028200000000     C                   PARM                    DSMSER
028300000000     C*
028400000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
028500000000     C                   CALL      'XKEY'
028600000000     C                   PARM                    DSKEY
028700000000     C*
028800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
028900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
029000000000     C*
029100000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
029200000000     C*   -NOME PGM CHIAMANTE
029300000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
029400060520     C                   MOVEL     'TISIE4R'     DSMSPG
029500060520     C                   MOVEL     'TISIE4R'     MSGQUE
029600000000     C                   MOVEL     *BLANKS       MSGKEY
029700000000     C                   MOVEL     *BLANKS       DSMSMS
029800000000     C*
029900000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
030000000000     C                   Z-ADD     0             FUNGRU            5 0
030100000000     C*
030200000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
030300000000     C                   EXSR      FUNGES
030400000000     C*
030500000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
030600000000     C                   MOVEL     '0'           F12L00            1
030700000000     C                   MOVEL     '0'           F03L00            1
030800000000     C                   MOVEL     '0'           FINL00            1
030900000000     C                   MOVEL     '0'           ERRL00            1
031000000000     C                   MOVEL     '0'           FLFINE            1
031100000000     C*
031200000000     C* LANCIO ROUTINE PER VERIFICARE SE
031300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
031400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
031500020919     C                   EXSR      REPDATIUTE
031600060520     C*
031700060520     C* IMPOSTA LA DATA CORRENTE
031800060520     C                   TIME                    N14              14 0
031900060520     C                   MOVE      N14           N8                8 0
032000060520     C                   Z-ADD     N8            G08DAT
032100060520     C                   Z-ADD     *ZEROS        G08INV
032200060520     C                   MOVEL     '0'           G08ERR
032300060520     C                   CALL      'XSRDA8'
032400060520     C                   PARM                    WLBDA8
032500060520     C                   Z-ADD     G08INV        DATCOR            8 0
032600060520     C*
032700060520     C* INIZIALIZZAZIONE VIDEO
032800060520     C                   CLEAR                   TISIE3DS
032900060520     C                   EVAL      ISIE3EXC = 'N'
033000060520     C                   Z-ADD     DATCOR        ISIE3DRI
033100060721     C                   EVAL      VIDDBG = 'N'
033200000000     C*
033300000000     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
033600000000     C*------------------------------------------------------------------------*
033700000000     C     FUNGES        BEGSR
033800000000     C*
033900000000     C* PULISCO CAMPI DI LAVORO
034000000000     C                   Z-ADD     0             W                 5 0
034100000000     C*
034200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
034400000000     C     1             DO        24            I
034500000000     C                   MOVEL     FUD(I)        FUDRIG
034600000000     C*
034700000000     C* PULIZIA FUNZIONI PRECEDENTI
034800000000     C                   MOVEL     *BLANKS       FUV(I)
034900000000     C*
035000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
035100000000     C* (SALTA F24)
035200000000     C     FUDATT        IFEQ      'S'
035300000000     C     FUDFUN        ANDNE     '24'
035400000000     C                   ADD       1             W
035500000000     C                   MOVEL     FUDDES        FUV(W)
035600000000     C                   END
035700000000     C                   END                                                    DO WHILE
035800000000     C*
035900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036000000000     C* ALTRIMENTI DISATTIVO F24
036100000000     C     1             DO        24            I
036200000000     C                   MOVEL     FUD(I)        FUDRIG
036300000000     C*
036400000000     C     FUDFUN        IFEQ      '24'
036500000000     C     W             IFGT      6
036600000000     C                   MOVEL     'S'           FUDATT
036700000000     C                   ELSE
036800000000     C                   MOVEL     *BLANKS       FUDATT
036900000000     C                   END                                                    W GT 8
037000000000     C                   MOVEL     FUDRIG        FUD(I)
037100000000     C                   END
037200000000     C                   END                                                    DO WHILE
037300000000     C*
037400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
037500000000     C     W             DIV       6             I
037600000000     C                   MVR                     Z
037700000000     C*
037800000000     C     Z             IFNE      0
037900000000     C     I             ADD       1             Z
038000000000     C                   ELSE
038100000000     C                   Z-ADD     I             Z                 5 0
038200000000     C                   END
038300000000     C*
038400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
038500000000     C                   ADD       1             FUNGRU
038600000000     C     FUNGRU        IFGT      Z
038700000000     C     FUNGRU        ORLT      1
038800000000     C                   Z-ADD     1             FUNGRU
038900000000     C                   END
039000000000     C*
039100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
039200000000     C     FUNGRU        IFEQ      1
039300000000     C                   MOVEL     FUV(1)        FUN001
039400000000     C                   MOVEL     FUV(2)        FUN002
039500000000     C                   MOVEL     FUV(3)        FUN003
039600000000     C                   MOVEL     FUV(4)        FUN004
039700000000     C                   MOVEL     FUV(5)        FUN005
039800000000     C                   MOVEL     FUV(6)        FUN006
039900000000     C                   END
040000000000     C*
040100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
040200000000     C     FUNGRU        IFEQ      2
040300000000     C                   MOVEL     FUV(7)        FUN001
040400000000     C                   MOVEL     FUV(8)        FUN002
040500000000     C                   MOVEL     FUV(9)        FUN003
040600000000     C                   MOVEL     FUV(10)       FUN004
040700000000     C                   MOVEL     FUV(11)       FUN005
040800000000     C                   MOVEL     FUV(12)       FUN006
040900000000     C                   END
041000000000     C*
041100000000     C* TERZO GRUPPO DI 6 FUNZIONI
041200000000     C     FUNGRU        IFEQ      3
041300000000     C                   MOVEL     FUV(13)       FUN001
041400000000     C                   MOVEL     FUV(14)       FUN002
041500000000     C                   MOVEL     FUV(15)       FUN003
041600000000     C                   MOVEL     FUV(16)       FUN004
041700000000     C                   MOVEL     FUV(17)       FUN005
041800000000     C                   MOVEL     FUV(18)       FUN006
041900000000     C                   END
042000000000     C*
042100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
042200000000     C     FUNGRU        IFEQ      4
042300000000     C                   MOVEL     FUV(19)       FUN001
042400000000     C                   MOVEL     FUV(20)       FUN002
042500000000     C                   MOVEL     FUV(21)       FUN003
042600000000     C                   MOVEL     FUV(22)       FUN004
042700000000     C                   MOVEL     *BLANKS       FUN005
042800000000     C                   MOVEL     *BLANKS       FUN006
042900000000     C                   END
043000000000     C*
043100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
043200000000     C                   MOVEL     *BLANKS       FUN007
043300000000     C                   MOVEL     *BLANKS       FUN008
043400000000     C*
043500000000     C     1             DO        24            I
043600000000     C                   MOVEL     FUD(I)        FUDRIG
043700000000     C     FUDFUN        IFEQ      '24'
043800000000     C     FUDATT        ANDEQ     'S'
043900000000     C                   MOVEL     FUDDES        FUN008
044000000000     C                   END
044100000000     C                   END                                                    DO
044200000000     C                   ENDSR
044300000000     C*------------------------------------------------------------------------*
044400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
044500000000     C*------------------------------------------------------------------------*
044600000000     C     FUNCON        BEGSR
044700000000     C*
044800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
044900000000     C                   EXSR      FUNVAR
045000000000     C*
045100000000     C* PULISCO CAMPI DI LAVORO
045200000000     C                   Z-ADD     0             I                 5 0
045300000000     C                   Z-ADD     0             W                 5 0
045400000000     C*
045500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
045600000000     C     FUNKEY        IFNE      *BLANKS
045700000000     C     1             DO        24            I
045800000000     C                   MOVEL     FUD(I)        FUDRIG
045900000000     C*
046000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
046100000000     C     FUNKEY        IFEQ      FUDFUN
046200000000     C     FUDATT        ANDEQ     'S'
046300000000     C                   Z-ADD     I             W
046400000000     C                   END
046500000000     C                   END                                                    DO WHILE
046600000000     C*
046700000000     C* FUNZIONE NON ATTIVA (ERRORE)
046800000000     C     W             IFEQ      0
046900000000     C*
047000000000     C* EMISSIONE MESSAGGIO DI ERRORE
047100000000     C                   MOVEL     MSG(1)        DSMSMS
047200000000     C*
047300000000     C                   END                                                    W=0
047400000000     C                   END                                                    FUNKEY NE *BLANKS
047500000000     C*
047600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
047700000000     C     W             IFNE      0
047800000000     C     FUNKEY        OREQ      *BLANKS
047900000000     C*
048000000000     C     FUNKEY        IFEQ      *BLANKS
048100000000     C                   EXSR      ENTGES
048200000000     C                   END
048300000000     C     FUNKEY        IFEQ      '01'
048400000000     C                   END
048500000000     C     FUNKEY        IFEQ      '03'
048600000000     C                   EXSR      F03GES
048700000000     C                   END
048800000000     C     FUNKEY        IFEQ      '06'
048900000000     C                   END
049000000000     C     FUNKEY        IFEQ      '07'
049100000000     C                   END
049200000000     C     FUNKEY        IFEQ      '08'
049300000000     C                   END
049400000000     C     FUNKEY        IFEQ      '12'
049500000000     C                   EXSR      F12GES
049600000000     C                   END
049700000000     C     FUNKEY        IFEQ      '21'
049800000000     C                   END
049900000000     C     FUNKEY        IFEQ      '22'
050000000000     C                   END
050100000000     C     FUNKEY        IFEQ      '24'
050200000000     C                   EXSR      F24GES
050300000000     C                   END
050400000000     C     FUNKEY        IFEQ      'HP'
050500000000     C                   END
050600000000     C*
050700000000     C                   END
050800000000     C*
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051200000000     C*------------------------------------------------------------------------*
051300000000     C     FUNVAR        BEGSR
051400000000     C*
051500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
051600000000     C*
051700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
051800000000     C     KEY           IFEQ      ENTER
051900000000     C                   MOVEL     *BLANKS       FUNKEY
052000000000     C                   END
052100000000     C     KEY           IFEQ      F1
052200000000     C                   MOVEL     '01'          FUNKEY
052300000000     C                   END
052400000000     C     KEY           IFEQ      F2
052500000000     C                   MOVEL     '02'          FUNKEY
052600000000     C                   END
052700000000     C     KEY           IFEQ      F3
052800000000     C                   MOVEL     '03'          FUNKEY
052900000000     C                   END
053000000000     C     KEY           IFEQ      F4
053100000000     C                   MOVEL     '04'          FUNKEY
053200000000     C                   END
053300000000     C     KEY           IFEQ      F5
053400000000     C                   MOVEL     '05'          FUNKEY
053500000000     C                   END
053600991103     C     KEY           IFEQ      F6
053700000000     C                   MOVEL     '06'          FUNKEY
053800000000     C                   END
053900000000     C     KEY           IFEQ      F7
054000000000     C                   MOVEL     '07'          FUNKEY
054100000000     C                   END
054200000000     C     KEY           IFEQ      F8
054300000000     C                   MOVEL     '08'          FUNKEY
054400000000     C                   END
054500000000     C     KEY           IFEQ      F9
054600000000     C                   MOVEL     '09'          FUNKEY
054700000000     C                   END
054800000000     C     KEY           IFEQ      F10
054900000000     C                   MOVEL     '10'          FUNKEY
055000000000     C                   END
055100000000     C     KEY           IFEQ      F11
055200000000     C                   MOVEL     '11'          FUNKEY
055300000000     C                   END
055400000000     C     KEY           IFEQ      F12
055500000000     C                   MOVEL     '12'          FUNKEY
055600000000     C                   END
055700000000     C     KEY           IFEQ      F13
055800000000     C                   MOVEL     '13'          FUNKEY
055900000000     C                   END
056000000000     C     KEY           IFEQ      F14
056100000000     C                   MOVEL     '14'          FUNKEY
056200000000     C                   END
056300000000     C     KEY           IFEQ      F15
056400000000     C                   MOVEL     '15'          FUNKEY
056500000000     C                   END
056600000000     C     KEY           IFEQ      F16
056700000000     C                   MOVEL     '16'          FUNKEY
056800000000     C                   END
056900000000     C     KEY           IFEQ      F17
057000000000     C                   MOVEL     '17'          FUNKEY
057100000000     C                   END
057200000000     C     KEY           IFEQ      F18
057300000000     C                   MOVEL     '18'          FUNKEY
057400000000     C                   END
057500000000     C     KEY           IFEQ      F19
057600000000     C                   MOVEL     '19'          FUNKEY
057700000000     C                   END
057800000000     C     KEY           IFEQ      F20
057900000000     C                   MOVEL     '20'          FUNKEY
058000000000     C                   END
058100000000     C     KEY           IFEQ      F21
058200000000     C                   MOVEL     '21'          FUNKEY
058300000000     C                   END
058400000000     C     KEY           IFEQ      F22
058500000000     C                   MOVEL     '22'          FUNKEY
058600000000     C                   END
058700000000     C     KEY           IFEQ      F23
058800000000     C                   MOVEL     '23'          FUNKEY
058900000000     C                   END
059000000000     C     KEY           IFEQ      F24
059100000000     C                   MOVEL     '24'          FUNKEY
059200000000     C                   END
059300000000     C     KEY           IFEQ      ROLLUP
059400000000     C                   MOVEL     'RU'          FUNKEY
059500000000     C                   END
059600000000     C     KEY           IFEQ      HELP
059700000000     C                   MOVEL     'HP'          FUNKEY
059800000000     C                   END
059900000000     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* NOTE PER COMPILAZIONE SCHIERE
060200000000     C*------------------------------------------------------------------------*
060300000000     C* -MSG-
060400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060500000000     C* (NON TOCCARE MSG,1)
060600000000     C*
060700000000     C* -CMD-
060800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
060900000000     C*
061000000000     C* -FUD-
061100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
061200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
061500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
061600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
061700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
061800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
061900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
062000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
062100000000     C*
062200000000     C*------------------------------------------------------------------------*
062300991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062400060520TISIE4R- Tasto funzionale non gestito.                                         1
062500060520TISIE4R- File Versioni "CAPPARIO DPD" vuoto o data non coperta da versione.    2
062600060520TISIE4R- Campo obbligatorio, inserire una data.                                3
062700060520TISIE4R- Data errata.                                                          4
062800060520TISIE4R- Campo obbligatorio, inserire il CAP                                   5
062900060520TISIE4R-                                                                       6
063000060520TISIE4R-                                                                       7
063100060520TISIE4R-                                                                       8
063200060520TISIE4R-                                                                       9
063300060520TISIE4R-                                                                      10
063400060520TISIE4R-                                                                      11
063500060520TISIE4R-                                                                      12
063600060520TISIE4R-                                                                      13
063700060520TISIE4R-                                                                      14
063800060520TISIE4R-                                                                      15
063900060520TISIE4R-                                                                      16
064000060520TISIE4R-                                                                      17
064100060520TISIE4R-                                                                      18
064200060520TISIE4R-                                                                      19
064300060520TISIE4R-                                                                      20
064400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
064500000000                   -        (S/N)=N                        funz.01¬
064600000000                   -        (S/N)=N                        funz.02¬
064700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
064800000000                   -        (S/N)=N                        funz.04¬
064900000000                   -        (S/N)=N                        funz.05¬
065000000216F06=Importazione   -        (S/N)=N                        funz.06¬
065100000104                   -        (S/N)=N                        funz.07¬
065200000105                   -        (S/N)=N                        funz.08¬
065300000000                   -        (S/N)=N                        funz.09¬
065400000000                   -        (S/N)=N                        funz.10¬
065500000000                   -        (S/N)=N                        funz.11¬
065600991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
065700000000                   -        (S/N)=N                        funz.13¬
065800000000                   -        (S/N)=N                        funz.14¬
065900000000                   -        (S/N)=N                        funz.15¬
066000000000                   -        (S/N)=N                        funz.16¬
066100000000                   -        (S/N)=N                        funz.17¬
066200000000                   -        (S/N)=N                        funz.18¬
066300000000                   -        (S/N)=N                        funz.19¬
066400000000                   -        (S/N)=N                        funz.20¬
066500000000                   -        (S/N)=N                        funz.21¬
066600000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
066700000000                   -        (S/N)=N                        funz.23¬
066800991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
